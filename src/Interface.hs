-- This module defines the C API required to be implemented for the ISS.
module Interface (
    uint32,
    uint16,
    uint8,
    int32,
    int16,
    int8,
    readReg,
    writeReg,
    readPC,
    writePC,
    ValueSize (..),
    load,
    store,
    InstrPart (..),
    instrPart,
) where

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

-- Size of a value that should be stored/loaded.
data ValueSize = Byte | Half | Word

load' :: String -> Bindings -> CExpr -> CExpr -> CExpr
load' funcName binds core addr = funcCall funcIdent [core, addr]
  where
    funcIdent = getIdent funcName binds

-- Load a value of the given size from the memory of the given core at the given address.
--
-- Interface contract:
--
--  uint8_t load_byte(void *core, uint32_t addr);
--  uint16_t load_half(void *core, uint32_t addr);
--  uint32_t load_word(void *core, uint32_t addr);
--
load :: ValueSize -> Bindings -> CExpr -> CExpr -> CExpr
load Byte = load' "load_byte"
load Half = load' "load_half"
load Word = load' "load_word"

store' :: String -> Bindings -> CExpr -> CExpr -> CExpr -> CExpr
store' funcName binds core addr value = funcCall funcIdent [core, addr, value]
  where
    funcIdent = getIdent funcName binds

-- Store a given value of the given size in the memory of the given core at a given address.
--
-- Interface contract:
--
--  void store_byte(void *core, uint32_t addr, uint8_t value);
--  void store_half(void *core, uint32_t addr, uint16_t value);
--  void store_word(void *core, uint32_t addr, uint32_t value);
--
store :: ValueSize -> Bindings -> CExpr -> CExpr -> CExpr -> CExpr
store Byte = store' "store_byte"
store Half = store' "store_half"
store Word = store' "store_word"

------------------------------------------------------------------------

-- Information that can be extracted from an instruction.
data InstrPart = RD | RS1 | RS2 | ImmI | ImmS | ImmB | ImmU | ImmJ | Shamt

instrPart' :: String -> CExpr -> Bindings -> CExpr
instrPart' funcName instr binds = funcCall funcIdent [instr]
  where
    funcIdent = getIdent funcName binds

-- Extract information from a decoded instruction.
--
-- Interface contract:
--
--  uint32_t instr_rs1(void *instr);
--  uint32_t instr_rs2(void *instr);
--  uint32_t instr_rd(void *instr);
--  uint32_t instr_immI(void *instr);
--  uint32_t instr_immS(void *instr);
--  uint32_t instr_immB(void *instr);
--  uint32_t instr_immU(void *instr);
--  uint32_t instr_immJ(void *instr);
--  uint32_t instr_shamt(void *instr);
--
instrPart :: InstrPart -> CExpr -> Bindings -> CExpr
instrPart RD = instrPart' "instr_rd"
instrPart RS1 = instrPart' "instr_rs1"
instrPart RS2 = instrPart' "instr_rs2"
instrPart ImmI = instrPart' "instr_immI"
instrPart ImmS = instrPart' "instr_immS"
instrPart ImmB = instrPart' "instr_immB"
instrPart ImmU = instrPart' "instr_immU"
instrPart ImmJ = instrPart' "instr_immJ"
instrPart Shamt = instrPart' "instr_shamt"
