module Main where

import Language.C
import LibRISCV.Spec.Expr
import Expression

main :: IO ()
main = do
    let result = runExpression $ (Eq (AddU (FromInt 42) (FromInt 23)) (FromInt 1337))
    putStrLn $ "Expr: " ++ show (pretty result)
