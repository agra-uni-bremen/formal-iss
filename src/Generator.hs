{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Generator (generate) where

import Language.C

import Control.Monad.Freer
import Control.Monad.Freer.Writer

import LibRISCV.Decoder (InstructionType)
import LibRISCV.Spec.AST (buildInstruction'')
import LibRISCV.Spec.Operations
import qualified LibRISCV.Spec.Expr as E

import Executor
import qualified Interface as IF

-- TODO: Treat inner value as unsigned
-- TODO: Don't create NodePos explicitly
evalE :: E.Expr CExpr -> CExpr
evalE (E.FromImm e)  = e
evalE (E.FromInt v)  = let cint = CInteger (fromIntegral v) HexRepr noFlags in
                                CConst (CIntConst cint $ mkNodeInfoOnlyPos nopos)
evalE (E.AddU e1 e2) = CBinary CAddOp (evalE e1) (evalE e2) $ mkNodeInfoOnlyPos nopos
evalE (E.AddS e1 e2) = CBinary CAddOp (evalE e1) (evalE e2) $ mkNodeInfoOnlyPos nopos
evalE (E.Eq e1 e2)   = CBinary CEqOp  (evalE e1) (evalE e2) $ mkNodeInfoOnlyPos nopos
evalE _            = error "not implemented"

------------------------------------------------------------------------

-- TODO: Emit a C function definition for the given instruction type.
generate' :: Eff '[Operations CExpr] w -> [CStat]
generate' req = snd $ run (runWriter (reinterpret gen req))
  where
    gen :: Operations CExpr ~> Eff '[Writer [CStat]]
    gen (GetRD  instr)          = pure $ IF.instrRD instr
    gen (GetRS1 instr)          = pure $ IF.instrRS1 instr
    gen (GetRS2 instr)          = pure $ IF.instrRS2 instr
    gen (ReadRegister idx)      = pure $ IF.readReg idx
    gen (WriteRegister idx val) = tell [CExpr (Just $ IF.writeReg idx (evalE val)) $ mkNodeInfoOnlyPos nopos]
    -- gen (ReadRegister _) = tell [CBreak $ mkNodeInfoOnlyPos nopos] >> pure (E.FromInt 0)
    gen _ = error "not implemented"

generate :: InstructionType -> CFunDef
generate inst = makeExecutor (mkIdent nopos "exec_add" (IF.names !! 23992)) block
  where
    cflow :: Eff '[Operations CExpr] ()
    cflow = buildInstruction'' @CExpr (CVar instrIdent (mkNodeInfoOnlyPos nopos)) inst

    block :: CStat
    block = CCompound [] (map CBlockStmt $ generate' cflow) (mkNodeInfoOnlyPos nopos)
