{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Generator (generate) where

import Language.C

import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Writer

import LibRISCV.Decoder (InstructionType)
import LibRISCV.Spec.AST (instrSemantics)
import LibRISCV.Spec.Operations
import qualified LibRISCV.Spec.Expr as E

import Bindings
import Executor
import qualified Interface as IF

-- TODO: Treat inner value as unsigned
-- TODO: Don't create NodePos explicitly
evalE :: E.Expr CExpr -> CExpr
evalE (E.FromImm e)  = e
evalE (E.FromInt v)  = let cint = CInteger (fromIntegral v) HexRepr noFlags in
                                CConst (CIntConst cint undefNode)
evalE (E.AddU e1 e2) = CBinary CAddOp (evalE e1) (evalE e2) undefNode
evalE (E.AddS e1 e2) = CBinary CAddOp (evalE e1) (evalE e2) undefNode
evalE (E.Eq e1 e2)   = CBinary CEqOp  (evalE e1) (evalE e2) undefNode
evalE _              = error "not implemented"

------------------------------------------------------------------------

buildSemantics :: Bindings -> Eff '[Operations CExpr] w -> [CStat]
buildSemantics binds req = snd $ run (runWriter (runReader binds (reinterpret2 gen req)))
  where
    gen :: Operations CExpr ~> Eff '[Reader Bindings, Writer [CStat]]
    gen (GetRD  instr)          = IF.instrRD instr  <$> asks (getIdent "instr_rd")
    gen (GetRS1 instr)          = IF.instrRS1 instr <$> asks (getIdent "instr_rs1")
    gen (GetRS2 instr)          = IF.instrRS2 instr <$> asks (getIdent "instr_rs2")
    gen (ReadRegister idx)      = ((flip) IF.readReg) idx <$> asks (getIdent "read_register")
    gen (WriteRegister idx val) = do
        ident <- asks (getIdent "write_register")
        tell [CExpr (Just $ IF.writeReg ident idx (evalE val)) undefNode]
    -- gen (ReadRegister _) = tell [CBreak undefNode] >> pure (E.FromInt 0)
    gen _ = error "not implemented"

generate :: [Name] -> InstructionType -> CFunDef
generate (nFunc:nInstr:ns) inst = makeExecutor funcIdent instrIdent block
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
