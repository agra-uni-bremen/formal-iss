{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Generator (generate, evalE) where

import Bindings
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Writer
import Conversion
import Data.Word
import Executor
import Interface qualified as IF
import Language.C
import LibRISCV.Decoder.Opcode (InstructionType)
import LibRISCV.Spec.AST (instrSemantics)
import LibRISCV.Spec.Expr qualified as E
import LibRISCV.Spec.Operations
import Util

-- XXX: This is a hack the Conversion type constraint needs
-- to be removed from instrSemantics (presently needed for Branches).
instance Conversion CExpr Word32 where
    convert = error "Word32 conversion not implemented"

-- Helper function to perform type conversions via casts.
castTo :: CDeclSpec -> CExpr -> CExpr
castTo ty expr = CCast (CDecl [ty] [] undefNode) expr undefNode

-- Assumption: CExpr is an unsigned value in two's complement (uint32_t).
evalE :: Bindings -> E.Expr CExpr -> CExpr
evalE _ (E.FromImm e) = e
evalE _ (E.FromUInt v) =
    let cint = CInteger (fromIntegral v) HexRepr noFlags
     in CConst (CIntConst cint undefNode)
evalE b (E.ZExtByte v) = castTo (IF.uint8 b) (evalE b v)
evalE b (E.ZExtHalf v) = castTo (IF.uint16 b) (evalE b v)
evalE b (E.SExtByte v) = castTo (IF.int8 b) (evalE b v)
evalE b (E.SExtHalf v) = castTo (IF.int16 b) (evalE b v)
evalE b (E.Add e1 e2) = CBinary CAddOp (evalE b e1) (evalE b e2) undefNode
evalE b (E.Sub e1 e2) = CBinary CSubOp (evalE b e1) (evalE b e2) undefNode
evalE b (E.Eq e1 e2) = CBinary CEqOp (evalE b e1) (evalE b e2) undefNode
evalE b (E.Slt e1 e2) =
    CBinary
        CLeOp
        (castTo (IF.int32 b) (evalE b e1))
        (castTo (IF.int32 b) (evalE b e2))
        undefNode
evalE b (E.Sge e1 e2) =
    CBinary
        CGeqOp
        (castTo (IF.int32 b) (evalE b e1))
        (castTo (IF.int32 b) (evalE b e2))
        undefNode
evalE b (E.Ult e1 e2) = CBinary CLeOp (evalE b e1) (evalE b e2) undefNode
evalE b (E.Uge e1 e2) = CBinary CGeqOp (evalE b e1) (evalE b e2) undefNode
evalE b (E.And e1 e2) = CBinary CAndOp (evalE b e1) (evalE b e2) undefNode
evalE b (E.Or e1 e2) = CBinary COrOp (evalE b e1) (evalE b e2) undefNode
evalE b (E.Xor e1 e2) = CBinary CXorOp (evalE b e1) (evalE b e2) undefNode
evalE _ _ = error "not implemented"

------------------------------------------------------------------------

buildSemantics :: Bindings -> Eff '[Operations CExpr] w -> [CBlockItem]
buildSemantics binds req = snd $ run (runWriter (runReader binds (reinterpret2 gen req)))
  where
    gen :: Operations CExpr ~> Eff '[Reader Bindings, Writer [CBlockItem]]
    gen (DecodeRD instr) = IF.instrRD instr <$> ask
    gen (DecodeRS1 instr) = IF.instrRS1 instr <$> ask
    gen (DecodeRS2 instr) = IF.instrRS2 instr <$> ask
    gen (DecodeImmI instr) = IF.instrImmI instr <$> ask
    gen (DecodeImmS instr) = IF.instrImmS instr <$> ask
    gen (DecodeImmB instr) = IF.instrImmB instr <$> ask
    gen (DecodeImmU instr) = IF.instrImmU instr <$> ask
    gen (DecodeImmJ instr) = IF.instrImmJ instr <$> ask
    gen (ReadRegister idx) = flip IF.readReg idx <$> ask
    gen (WriteRegister idx val) = do
        curBinds <- ask
        let expr = IF.writeReg curBinds idx (evalE curBinds val)
        tell [CBlockStmt $ CExpr (Just expr) undefNode]
    gen (LoadWord addr) = do
        curBinds <- ask
        pure $ IF.loadWord curBinds (evalE curBinds addr)
    gen (StoreWord addr value) = do
        curBinds <- ask
        let expr = IF.storeWord curBinds (evalE curBinds addr) (evalE curBinds value)
        tell [CBlockStmt $ CExpr (Just expr) undefNode]
    gen ReadPC = do
        curBinds <- ask
        let pc = IF.readPC curBinds

        let ident = getIdent "link" curBinds
        let declr = CDeclr (Just ident) [] Nothing [] undefNode
        let var = CDecl [IF.uint32 curBinds] [(Just declr, Just (CInitExpr pc undefNode), Nothing)] undefNode

        tell [CBlockDecl var]
        pure $ CVar ident undefNode
    gen (WritePC value) = do
        curBinds <- ask
        let expr = IF.writePC binds (evalE curBinds value)
        tell [CBlockStmt $ CExpr (Just expr) undefNode]
    gen _ = error "not implemented"

generate' :: [Name] -> InstructionType -> (CFunDef, [Name])
generate' (nFunc : nInstr : nPC : ns) inst = (makeExecutor funcIdent funcArgs block, newNs)
  where
    (bindings, newNs) = mkBindings ns

    -- Identifier for the generated function itself.
    funcIdent :: Ident
    funcIdent = mkIdent nopos ("exec_" ++ foldcase (show inst)) nFunc

    -- Identifier for the instruction argument of the function.
    instrIdent :: Ident
    instrIdent = mkIdent nopos "instr" nInstr

    -- Identifier for the current program counter.
    pcIdent :: Ident
    pcIdent = mkIdent nopos "curPC" nPC

    -- Function arguments for the executor.
    funcArgs :: [CDecl]
    funcArgs = [pcArg bindings pcIdent, instrArg instrIdent]

    cflow :: Eff '[Operations CExpr] ()
    cflow = instrSemantics @CExpr (CVar pcIdent undefNode) (CVar instrIdent undefNode) inst

    block :: CStat
    block = CCompound [] (buildSemantics bindings cflow) undefNode
generate' _ _ = error "invalid name list"

generate :: [InstructionType] -> [CFunDef]
generate types = snd $ foldl fn (newNameSupply, []) types
  where
    -- Generate a CFunDef for each instruction and propagate the name supply.
    fn (ns, funcs) ty = let (f, newNs) = generate' ns ty in (newNs, f : funcs)
