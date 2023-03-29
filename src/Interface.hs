-- This module defines the C API required to be implemented for the ISS.
module Interface where

import Bindings
import Language.C
import Util

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
--  uint32_t read_register(void *core, unsigned idx);
--
readReg :: Bindings -> CExpr -> CExpr -> CExpr
readReg binds core idx = funcCall funcIdent [core, idx]
  where
    funcIdent = getIdent "read_register" binds

-- Interface contract:
--
--  void write_register(void *core, unsigned idx, uint32_t val)
--
writeReg :: Bindings -> CExpr -> CExpr -> CExpr -> CExpr
writeReg binds core idx val = funcCall funcIdent [core, idx, val]
  where
    funcIdent = getIdent "write_register" binds

------------------------------------------------------------------------

-- Interface contract:
--
--  uint32_t read_next_pc(void *core)
--
readPC :: Bindings -> CExpr -> CExpr
readPC binds core = funcCall funcIdent [core]
  where
    funcIdent = getIdent "read_next_pc" binds

-- Interface contract:
--
--  void write_pc(void *core, uint32_t newPC)
--
writePC :: Bindings -> CExpr -> CExpr -> CExpr
writePC binds core value = funcCall funcIdent [core, value]
  where
    funcIdent = getIdent "write_pc" binds

------------------------------------------------------------------------

-- Interface contract:
--
--  uint8_t load_byte(uint32_t addr)
--
loadByte :: Bindings -> CExpr -> CExpr -> CExpr
loadByte binds core addr = funcCall funcIdent [core, addr]
  where
    funcIdent = getIdent "load_byte" binds

-- Interface contract:
--
--  uint16_t load_half(uint32_t addr)
--
loadHalf :: Bindings -> CExpr -> CExpr -> CExpr
loadHalf binds core addr = funcCall funcIdent [core, addr]
  where
    funcIdent = getIdent "load_half" binds

-- Interface contract:
--
--  uint32_t load_word(uint32_t addr)
--
loadWord :: Bindings -> CExpr -> CExpr -> CExpr
loadWord binds core addr = funcCall funcIdent [core, addr]
  where
    funcIdent = getIdent "load_word" binds

-- Interface contract:
--
--  void store_byte(uint32_t addr, uint8_t value)
--
storeByte :: Bindings -> CExpr -> CExpr -> CExpr -> CExpr
storeByte binds core addr value = funcCall funcIdent [core, addr, value]
  where
    funcIdent = getIdent "store_byte" binds

-- Interface contract:
--
--  void store_half(uint32_t addr, uint16_t value)
--
storeHalf :: Bindings -> CExpr -> CExpr -> CExpr -> CExpr
storeHalf binds core addr value = funcCall funcIdent [core, addr, value]
  where
    funcIdent = getIdent "store_half" binds

-- Interface contract:
--
--  void store_word(uint32_t addr, uint32_t value)
--
storeWord :: Bindings -> CExpr -> CExpr -> CExpr -> CExpr
storeWord binds core addr value = funcCall funcIdent [core, addr, value]
  where
    funcIdent = getIdent "store_word" binds

------------------------------------------------------------------------

getInstrPart :: String -> CExpr -> Bindings -> CExpr
getInstrPart funcName instr binds = funcCall funcIdent [instr]
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
