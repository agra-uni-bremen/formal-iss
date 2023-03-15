{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Generator (generate, evalE) where

import Bindings
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Data.Maybe
import Executor
import Interface qualified as IF
import Language.C
import LibRISCV.Decoder.Opcode (InstructionType)
import LibRISCV.Spec.AST (instrSemantics)
import LibRISCV.Spec.Expr qualified as E
import LibRISCV.Spec.Operations
import Statement (Statement, runStatement)
import Statement qualified as S
import Util

-- Assumption: CExpr is an unsigned value in two's complement (uint32_t).
evalE :: Bindings -> E.Expr CExpr -> CExpr
evalE _ (E.FromImm e) = e
evalE _ (E.FromUInt v) =
    let cint = CInteger (fromIntegral v) HexRepr noFlags
     in CConst (CIntConst cint undefNode)
evalE b (E.ZExtByte v) = castTo (IF.uint32 b) $ castTo (IF.uint8 b) (evalE b v)
evalE b (E.ZExtHalf v) = castTo (IF.uint32 b) $ castTo (IF.uint16 b) (evalE b v)
evalE b (E.SExtByte v) = do
    let i8 = castTo (IF.int8 b) (evalE b v)
    let i32 = castTo (IF.int32 b) i8
    castTo (IF.uint32 b) i32
evalE b (E.SExtHalf v) = do
    let i16 = castTo (IF.int16 b) (evalE b v)
    let i32 = castTo (IF.int32 b) i16
    castTo (IF.uint32 b) i32
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
evalE b (E.LShl e1 e2) = CBinary CShlOp (evalE b e1) (evalE b e2) undefNode
evalE b (E.LShr e1 e2) = CBinary CShrOp (evalE b e1) (evalE b e2) undefNode
-- XXX: This assumes that the compiler implements shift on signed values
-- using arithmetic shift which is true for GCC/clang but not standard-compliant.
evalE b (E.AShr e1 e2) = CBinary CShrOp (castTo (IF.int32 b) (evalE b e1)) (evalE b e2) undefNode

------------------------------------------------------------------------

-- Generate C code for the abstract semantics description of a given instruction from LibRISCV.
-- Since C is an imperative programming language we need to be able to emit both statements
-- and expressions. This is achieved by emitting statements as a "side effect" through
-- an algebraic State effect.
buildSemantics :: Bindings -> Eff '[Operations CExpr] w -> [CBlockItem]
buildSemantics binds req = snd $ run (runStatement (runReader binds (reinterpret2 gen req)))
  where
    gen :: Operations CExpr ~> Eff '[Reader Bindings, Statement]
    gen (DecodeRD instr) = IF.instrRD instr <$> ask
    gen (DecodeRS1 instr) = IF.instrRS1 instr <$> ask
    gen (DecodeRS2 instr) = IF.instrRS2 instr <$> ask
    gen (DecodeImmI instr) = IF.instrImmI instr <$> ask
    gen (DecodeImmS instr) = IF.instrImmS instr <$> ask
    gen (DecodeImmB instr) = IF.instrImmB instr <$> ask
    gen (DecodeImmU instr) = IF.instrImmU instr <$> ask
    gen (DecodeImmJ instr) = IF.instrImmJ instr <$> ask
    gen (DecodeShamt instr) = IF.instrShamt instr <$> ask
    gen (RunIf expr ifTrue) = do
        gen ifTrue
        trueBlock <- S.pop

        curBinds <- ask
        let cond = evalE curBinds expr
        let ifStat = CIf cond (CCompound [] [fromJust trueBlock] undefNode) Nothing undefNode

        S.push (CBlockStmt ifStat)
    gen (RunUnless expr unlessTrue) = do
        gen unlessTrue
        unlessBlock <- S.pop

        curBinds <- ask
        let cond = evalE curBinds expr
        let ifStat =
                CIf
                    cond
                    (CCompound [] [] undefNode)
                    (Just $ CCompound [] [fromJust unlessBlock] undefNode)
                    undefNode

        S.push (CBlockStmt ifStat)
    gen (ReadRegister idx) = flip IF.readReg idx <$> ask
    gen (WriteRegister idx val) = do
        curBinds <- ask
        let expr = IF.writeReg curBinds idx (evalE curBinds val)
        S.push (CBlockStmt $ CExpr (Just expr) undefNode)
    gen (LoadByte addr) = do
        curBinds <- ask
        pure $ IF.loadByte curBinds (evalE curBinds addr)
    gen (LoadHalf addr) = do
        curBinds <- ask
        pure $ IF.loadHalf curBinds (evalE curBinds addr)
    gen (LoadWord addr) = do
        curBinds <- ask
        pure $ IF.loadWord curBinds (evalE curBinds addr)
    gen (StoreByte addr value) = do
        curBinds <- ask
        let expr = IF.storeByte curBinds (evalE curBinds addr) (evalE curBinds value)
        S.push (CBlockStmt $ CExpr (Just expr) undefNode)
    gen (StoreHalf addr value) = do
        curBinds <- ask
        let expr = IF.storeHalf curBinds (evalE curBinds addr) (evalE curBinds value)
        S.push (CBlockStmt $ CExpr (Just expr) undefNode)
    gen (StoreWord addr value) = do
        curBinds <- ask
        let expr = IF.storeWord curBinds (evalE curBinds addr) (evalE curBinds value)
        S.push (CBlockStmt $ CExpr (Just expr) undefNode)
    gen ReadPC = do
        curBinds <- ask
        let pc = IF.readPC curBinds

        let ident = getIdent "link" curBinds
        let declr = CDeclr (Just ident) [] Nothing [] undefNode
        let var = CDecl [IF.uint32 curBinds] [(Just declr, Just (CInitExpr pc undefNode), Nothing)] undefNode

        S.push (CBlockDecl var)
        pure $ CVar ident undefNode
    gen (WritePC value) = do
        curBinds <- ask
        let expr = IF.writePC binds (evalE curBinds value)
        S.push (CBlockStmt $ CExpr (Just expr) undefNode)
    gen (Exception _ _) = do
        curBinds <- ask
        let abort = funcCall (getIdent "abort" curBinds) []
        S.push (CBlockStmt $ CExpr (Just abort) undefNode)
    gen (Ebreak _) = pure ()
    gen (Ecall _) = pure ()

generate' :: Bindings -> [Name] -> InstructionType -> (CFunDef, [Name])
generate' binds (nFunc : nInstr : nPC : ns) inst = (makeExecutor funcIdent funcArgs block, ns)
  where
    -- Identifier for the generated function itself.
    funcIdent :: Ident
    funcIdent = mkIdent nopos ("exec_" ++ foldcase (show inst)) nFunc

    -- Identifier for the instruction argument of the function.
    instrIdent :: Ident
    instrIdent = mkIdent nopos "instr" nInstr

    -- Identifier for the current program counter.
    pcIdent :: Ident
    pcIdent = mkIdent nopos "instrPC" nPC

    -- Function arguments for the executor.
    funcArgs :: [CDecl]
    funcArgs = [pcArg binds pcIdent, instrArg instrIdent]

    cflow :: Eff '[Operations CExpr] ()
    cflow = instrSemantics @CExpr (CVar pcIdent undefNode) (CVar instrIdent undefNode) inst

    block :: CStat
    block = CCompound [] (buildSemantics binds cflow) undefNode
generate' _ _ _ = error "invalid name list"

generate :: [InstructionType] -> [CFunDef]
generate types = snd $ foldl fn (initialNs, []) types
  where
    -- Generate a CFunDef for each instruction and propagate the name supply.
    fn (ns, funcs) ty = let (f, newNs) = generate' bindings ns ty in (newNs, f : funcs)

    -- Bindings for identifiers and initial name supply.
    (bindings, initialNs) = mkBindings newNameSupply
