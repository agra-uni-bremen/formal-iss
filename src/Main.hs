module Main where

import LibRISCV (RegIdx(..))
import LibRISCV.Decoder (InstructionType(..))

import Language.C
import Generator

main :: IO ()
main = do
    let inst = ADD Zero Zero Zero
    let stat = generate newNameSupply inst

    putStrLn $ show (pretty stat)
