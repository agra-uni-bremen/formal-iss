{-# LANGUAGE DataKinds #-}
module Generator (runExpression, generate) where

import Language.C

import Control.Monad.Freer
import Control.Monad.Freer.Writer

import LibRISCV.Decoder (InstructionType)
import LibRISCV.Spec.Operations
import qualified LibRISCV.Spec.Expr as E

-- TODO: Treat inner value as unsigned
-- TODO: Don't create NodePos explicitly
runExpression :: E.Expr CExpr -> CExpr
runExpression (E.FromImm e)  = e
runExpression (E.FromInt v)  = let cint = CInteger (fromIntegral v) HexRepr noFlags in
                                CConst (CIntConst cint $ mkNodeInfoOnlyPos nopos)
runExpression (E.AddU e1 e2) = CBinary CAddOp (runExpression e1) (runExpression e2) $ mkNodeInfoOnlyPos nopos
runExpression (E.Eq e1 e2)   = CBinary CEqOp  (runExpression e1) (runExpression e2) $ mkNodeInfoOnlyPos nopos
runExpression _            = error "not implemented"

------------------------------------------------------------------------

generate :: InstructionType -> Eff '[Operations (E.Expr CExpr)] w -> [CStat]
generate _ req = snd $ run (runWriter (reinterpret gen req))
  where
    gen :: Operations (E.Expr CExpr) ~> Eff '[Writer [CStat]]
    gen _ = error "not implemented"
