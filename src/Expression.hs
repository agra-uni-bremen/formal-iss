module Expression (runExpression) where

import Language.C

import LibRISCV.Spec.Expr

-- TODO: Treat inner value as unsigned
-- TODO: Don't create NodePos explicitly
runExpression :: Expr CExpr -> CExpr
runExpression (FromImm e)  = e
runExpression (FromInt v)  = let cint = CInteger (fromIntegral v) HexRepr noFlags in
                                CConst (CIntConst cint $ mkNodeInfoOnlyPos nopos)
runExpression (AddU e1 e2) = CBinary CAddOp (runExpression e1) (runExpression e2) $ mkNodeInfoOnlyPos nopos
runExpression (Eq e1 e2)   = CBinary CEqOp  (runExpression e1) (runExpression e2) $ mkNodeInfoOnlyPos nopos
runExpression _            = error "not implemented"
