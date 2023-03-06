-- This module defines the C API required to be implemented for the ISS.
module Interface where

import Bindings
import Language.C

typedef :: String -> Bindings -> CDeclSpec
typedef s b = CTypeSpec (CTypeDef typeIdent undefNode)
  where
    typeIdent = getIdent s b

-- The typedef for uint32_t from stdint.h.
uint32 :: Bindings -> CDeclSpec
uint32 = typedef "uint32_t"

-- The typedef for uint32_t from stdint.h.
uint16 :: Bindings -> CDeclSpec
uint16 = typedef "uint16_t"

-- -- The typedef for uint8_t from stdint.h.
uint8 :: Bindings -> CDeclSpec
uint8 = typedef "uint8_t"

-- -- The typedef for int32_t from stdint.h.
int32 :: Bindings -> CDeclSpec
int32 = typedef "int32_t"

-- -- The typedef for int32_t from stdint.h.
int16 :: Bindings -> CDeclSpec
int16 = typedef "int16_t"

-- -- The typedef for int8_t from stdint.h.
int8 :: Bindings -> CDeclSpec
int8 = typedef "int8_t"

------------------------------------------------------------------------

-- Interface contract:
--
--  uint32_t read_register(unsigned idx);
--
readReg :: Bindings -> CExpr -> CExpr
readReg binds idx = CCall (CVar funcIdent undefNode) [idx] undefNode
  where
    funcIdent = getIdent "read_register" binds

-- Interface contract:
--
--  void write_register(unsigned idx, uint32_t val)
--
writeReg :: Bindings -> CExpr -> CExpr -> CExpr
writeReg binds idx val = CCall (CVar funcIdent undefNode) [idx, val] undefNode
  where
    funcIdent = getIdent "write_register" binds

------------------------------------------------------------------------

getInstrPart :: String -> CExpr -> Bindings -> CExpr
getInstrPart funcName instr binds = CCall (CVar funcIdent undefNode) [instr] undefNode
  where
    funcIdent = getIdent funcName binds

-- Interface contract:
--
--  uint32_t instr_rs1(void *instr);
--
instrRS1 :: CExpr -> Bindings -> CExpr
instrRS1 = getInstrPart "instr_rs1"

-- Interface contract:
--
--  uint32_t instr_rs2(void *instr);
--
instrRS2 :: CExpr -> Bindings -> CExpr
instrRS2 = getInstrPart "instr_rs2"

-- Interface contract:
--
--  uint32_t instr_rd(void *instr);
--
instrRD :: CExpr -> Bindings -> CExpr
instrRD = getInstrPart "instr_rd"

-- Interface contract:
--
--  uint32_t instr_immI(void *instr);
--
instrImmI :: CExpr -> Bindings -> CExpr
instrImmI = getInstrPart "instr_immI"

-- Interface contract:
--
--  uint32_t instr_immS(void *instr);
--
instrImmS :: CExpr -> Bindings -> CExpr
instrImmS = getInstrPart "instr_immS"

-- Interface contract:
--
--  uint32_t instr_immB(void *instr);
--
instrImmB :: CExpr -> Bindings -> CExpr
instrImmB = getInstrPart "instr_immB"

-- Interface contract:
--
--  uint32_t instr_immU(void *instr);
--
instrImmU :: CExpr -> Bindings -> CExpr
instrImmU = getInstrPart "instr_immU"

-- Interface contract:
--
--  uint32_t instr_immJ(void *instr);
--
instrImmJ :: CExpr -> Bindings -> CExpr
instrImmJ = getInstrPart "instr_immJ"
