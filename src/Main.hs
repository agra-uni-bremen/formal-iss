module Main where

import Generator
import Language.C
import LibRISCV (RegIdx (..))
import LibRISCV.Decoder (InstructionType (..))

main :: IO ()
main = do
    let inst = ADD Zero Zero Zero
    let stat = generate newNameSupply inst

    putStrLn $ show (pretty stat)
