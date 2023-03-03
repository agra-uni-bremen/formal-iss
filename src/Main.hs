module Main where

import Generator
import Language.C
import LibRISCV.Decoder.Opcode (InstructionType (..))

main :: IO ()
main = do
    let inst = ADD
    let stat = generate newNameSupply inst

    print (pretty stat)
