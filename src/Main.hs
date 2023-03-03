module Main where

import Generator
import Language.C
import LibRISCV.Decoder.Opcode (InstructionType (..))

main :: IO ()
main = do
    let stat = generate [ADD, AND]
    mapM print (map pretty stat)
    pure ()
