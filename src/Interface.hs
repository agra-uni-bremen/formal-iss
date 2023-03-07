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

-- Interface contract:
--
--  uint32_t read_next_pc(void)
--
readPC :: Bindings -> CExpr
readPC binds = CCall (CVar funcIdent undefNode) [] undefNode
  where
    funcIdent = getIdent "read_next_pc" binds

-- Interface contract:
--
--  void write_pc(uint32_t newPC)
--
writePC :: Bindings -> CExpr -> CExpr
writePC binds value = CCall (CVar funcIdent undefNode) [value] undefNode
  where
    funcIdent = getIdent "write_pc" binds

------------------------------------------------------------------------

-- Interface contract:
--
--  uint8_t load_byte(uint32_t addr)
--
loadByte :: Bindings -> CExpr -> CExpr
loadByte binds addr = CCall (CVar funcIdent undefNode) [addr] undefNode
  where
    funcIdent = getIdent "load_byte" binds

-- Interface contract:
--
--  uint16_t load_half(uint32_t addr)
--
loadHalf :: Bindings -> CExpr -> CExpr
loadHalf binds addr = CCall (CVar funcIdent undefNode) [addr] undefNode
  where
    funcIdent = getIdent "load_half" binds

-- Interface contract:
--
--  uint32_t load_word(uint32_t addr)
--
loadWord :: Bindings -> CExpr -> CExpr
loadWord binds addr = CCall (CVar funcIdent undefNode) [addr] undefNode
  where
    funcIdent = getIdent "load_word" binds

-- Interface contract:
--
--  void store_byte(uint32_t addr, uint8_t value)
--
storeByte :: Bindings -> CExpr -> CExpr -> CExpr
storeByte binds addr value = CCall (CVar funcIdent undefNode) [addr, value] undefNode
  where
    funcIdent = getIdent "store_byte" binds

-- Interface contract:
--
--  void store_half(uint32_t addr, uint16_t value)
--
storeHalf :: Bindings -> CExpr -> CExpr -> CExpr
storeHalf binds addr value = CCall (CVar funcIdent undefNode) [addr, value] undefNode
  where
    funcIdent = getIdent "store_half" binds

-- Interface contract:
--
--  void store_word(uint32_t addr, uint32_t value)
--
storeWord :: Bindings -> CExpr -> CExpr -> CExpr
storeWord binds addr value = CCall (CVar funcIdent undefNode) [addr, value] undefNode
  where
    funcIdent = getIdent "store_word" binds

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

-- Interface contract
--
--  uint32_t instr_shamt(void *instr);
instrShamt :: CExpr -> Bindings -> CExpr
instrShamt = getInstrPart "instr_shamt"
