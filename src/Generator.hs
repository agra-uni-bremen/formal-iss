{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Generator (generate) where

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

-- TODO: Treat inner value as unsigned
evalE :: E.Expr CExpr -> CExpr
evalE (E.FromImm e) = e
evalE (E.FromInt v) =
    let cint = CInteger (fromIntegral v) HexRepr noFlags
     in CConst (CIntConst cint undefNode)
evalE (E.AddU e1 e2) = CBinary CAddOp (evalE e1) (evalE e2) undefNode
evalE (E.AddS e1 e2) = CBinary CAddOp (evalE e1) (evalE e2) undefNode
evalE (E.Eq e1 e2) = CBinary CEqOp (evalE e1) (evalE e2) undefNode
evalE _ = error "not implemented"

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
        let expr = IF.writeReg curBinds idx (evalE val)
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
