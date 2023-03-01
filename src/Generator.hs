{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Generator (generate, evalE) where

import Bindings
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Writer
import Executor
import Interface qualified as IF
import Language.C
import LibRISCV.Decoder (InstructionType)
import LibRISCV.Spec.AST (instrSemantics)
import LibRISCV.Spec.Expr qualified as E
import LibRISCV.Spec.Operations

-- Helper function to perform type conversions via casts.
castTo :: CDeclSpec -> CExpr -> CExpr
castTo ty expr = CCast (CDecl [ty] [] undefNode) expr undefNode

-- Assumption: CExpr is an unsigned value in two's complement (uint32_t).
evalE :: Bindings -> E.Expr CExpr -> CExpr
evalE _ (E.FromImm e) = e
evalE _ (E.FromInt v) =
    let cint = CInteger (fromIntegral v) HexRepr noFlags
     in CConst (CIntConst cint undefNode)
evalE b (E.FromUInt v) = evalE b (E.FromInt $ fromIntegral v)
evalE b (E.ZExtByte v) = castTo (IF.uint8 b) (evalE b v)
evalE b (E.ZExtHalf v) = castTo (IF.uint16 b) (evalE b v)
evalE b (E.SExtByte v) = castTo (IF.int8 b) (evalE b v)
evalE b (E.SExtHalf v) = castTo (IF.int16 b) (evalE b v)
evalE b (E.AddU e1 e2) = CBinary CAddOp (evalE b e1) (evalE b e2) undefNode
evalE b (E.AddS e1 e2) = CBinary CAddOp (evalE b e1) (evalE b e2) undefNode
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

buildSemantics :: Bindings -> Eff '[Operations CExpr] w -> [CStat]
buildSemantics binds req = snd $ run (runWriter (runReader binds (reinterpret2 gen req)))
  where
    gen :: Operations CExpr ~> Eff '[Reader Bindings, Writer [CStat]]
    gen (GetRD instr) = IF.instrRD instr <$> ask
    gen (GetRS1 instr) = IF.instrRS1 instr <$> ask
    gen (GetRS2 instr) = IF.instrRS2 instr <$> ask
    gen (ReadRegister idx) = flip IF.readReg idx <$> ask
    gen (WriteRegister idx val) = do
        curBinds <- ask
        let expr = IF.writeReg curBinds idx (evalE curBinds val)
        tell [CExpr (Just expr) undefNode]
    gen _ = error "not implemented"

generate :: [Name] -> InstructionType -> CFunDef
generate (nFunc : nInstr : ns) inst = makeExecutor funcIdent instrIdent block
  where
    -- Identifier for the generated function itself.
    funcIdent :: Ident
    funcIdent = mkIdent nopos "exec_add" nFunc

    -- Identifier for the instruction argument of the function.
    instrIdent :: Ident
    instrIdent = mkIdent nopos "instr" nInstr

    bindings :: Bindings
    bindings = mkBindings ns

    cflow :: Eff '[Operations CExpr] ()
    cflow = instrSemantics @CExpr (CVar instrIdent undefNode) inst

    block :: CStat
    block = CCompound [] (map CBlockStmt $ buildSemantics bindings cflow) undefNode
generate _ _ = error "invalid name list"
