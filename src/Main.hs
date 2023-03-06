module Main where

import Generator
import Language.C
import LibRISCV.Decoder.Opcode (InstructionType (..))

-- Instruction from the RISC-V base instruction set.
baseInstr :: [InstructionType]
baseInstr =
    [ ADD
    , ADDI
    , LW
    , SW
    , JAL
    ]

------------------------------------------------------------------------

-- Create a transalation unit for a list of function definitions.
buildTransUnit :: [CFunDef] -> CTranslUnit
buildTransUnit funcs = CTranslUnit funcsExt undefNode
  where
    funcsExt :: [CExtDecl]
    funcsExt = map CFDefExt funcs

main :: IO ()
main = do
    let transUnit = buildTransUnit $ generate baseInstr
    print (pretty transUnit)
