-- This module defines the C API required to be implemented for the ISS.
module Interface where

import Bindings
import Language.C

-- The typedef for uint32_t from stdint.h.
uint32 :: Bindings -> CDeclSpec
uint32 binds = CTypeSpec (CTypeDef typeIdent undefNode)
  where
    typeIdent = getIdent "uint32_t" binds

-- The typedef for uint8_t from stdint.h.
uint8 :: Bindings -> CDeclSpec
uint8 binds = CTypeSpec (CTypeDef typeIdent undefNode)
  where
    typeIdent = getIdent "uint8_t" binds

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

getInstrReg :: String -> CExpr -> Bindings -> CExpr
getInstrReg funcName instr binds = CCall (CVar funcIdent undefNode) [instr] undefNode
  where
    funcIdent = getIdent funcName binds

-- Interface contract:
--
--  uint32_t instr_rs1(void *instr);
--
instrRS1 :: CExpr -> Bindings -> CExpr
instrRS1 = getInstrReg "instr_rs1"

-- Interface contract:
--
--  uint32_t instr_rs2(void *instr);
--
instrRS2 :: CExpr -> Bindings -> CExpr
instrRS2 = getInstrReg "instr_rs2"

-- Interface contract:
--
--  uint32_t instr_rd(void *instr);
--
instrRD :: CExpr -> Bindings -> CExpr
instrRD = getInstrReg "instr_rd"
