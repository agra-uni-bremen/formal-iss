{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Generator (runExpression, generate) where

import Language.C

import Control.Monad.Freer
import Control.Monad.Freer.Writer

import LibRISCV.Decoder (InstructionType)
import LibRISCV.Spec.Operations
import qualified LibRISCV.Spec.Expr as E

import qualified Interface as IF

type Evaluator = (E.Expr CExpr -> CExpr)

-- TODO: Treat inner value as unsigned
-- TODO: Don't create NodePos explicitly
runExpression :: E.Expr CExpr -> CExpr
runExpression (E.FromImm e)  = e
runExpression (E.FromInt v)  = let cint = CInteger (fromIntegral v) HexRepr noFlags in
                                CConst (CIntConst cint $ mkNodeInfoOnlyPos nopos)
runExpression (E.AddU e1 e2) = CBinary CAddOp (runExpression e1) (runExpression e2) $ mkNodeInfoOnlyPos nopos
runExpression (E.AddS e1 e2) = CBinary CAddOp (runExpression e1) (runExpression e2) $ mkNodeInfoOnlyPos nopos
runExpression (E.Eq e1 e2)   = CBinary CEqOp  (runExpression e1) (runExpression e2) $ mkNodeInfoOnlyPos nopos
runExpression _            = error "not implemented"

------------------------------------------------------------------------

-- TODO: Call buildInstruction'' for the instruction type.
-- TODO: Emit a C function definition for the given instruction type.
generate :: InstructionType -> Evaluator -> Eff '[Operations CExpr] w -> [CStat]
generate _ evalE req = snd $ run (runWriter (reinterpret gen req))
  where
    gen :: Operations CExpr ~> Eff '[Writer [CStat]]
    gen (GetRD  instr)          = pure $ IF.instrRD instr
    gen (GetRS1 instr)          = pure $ IF.instrRS1 instr
    gen (GetRS2 instr)          = pure $ IF.instrRS2 instr
    gen (ReadRegister idx)      = pure $ IF.readReg idx
    gen (WriteRegister idx val) = tell [CExpr (Just $ IF.writeReg idx (evalE val)) $ mkNodeInfoOnlyPos nopos]
    -- gen (ReadRegister _) = tell [CBreak $ mkNodeInfoOnlyPos nopos] >> pure (E.FromInt 0)
    gen _ = error "not implemented"
