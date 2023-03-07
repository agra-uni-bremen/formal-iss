module Main where

import Generator
import Language.C
import LibRISCV.Decoder.Opcode (InstructionType (..))
import System.Environment (getArgs)

-- Instruction from the RISC-V base instruction set.
baseInstr :: [InstructionType]
baseInstr =
    [ ADD
    , ADDI
    , AND
    , ANDI
    , AUIPC
    , BEQ
    , BGE
    , BGEU
    , BLT
    , BLTU
    , BNE
    , -- , EBREAK
      -- , ECALL
      FENCE
    , JAL
    , JALR
    , LB
    , LBU
    , LH
    , LHU
    , LUI
    , LW
    , OR
    , ORI
    , SB
    , SH
    , SLL
    , SLLI
    , SLT
    , SLTI
    , SLTIU
    , SLTU
    , SRA
    , SRAI
    , SRL
    , SRLI
    , SUB
    , SW
    , XOR
    , XORI
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

    args <- getArgs
    case args of
        filePath : _ -> writeFile filePath (show $ pretty transUnit)
        _ -> print (pretty transUnit)
