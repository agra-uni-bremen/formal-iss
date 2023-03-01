-- This module defines the C API required to be implemented for the ISS.
module Interface where

import Language.C

-- Interface contract:
--
--  uint32_t read_register(unsigned idx);
--
readReg :: Ident -> CExpr -> CExpr
readReg funcName idx = CCall (CVar funcName undefNode) [idx] undefNode

-- Interface contract:
--
--  void write_register(unsigned idx, uint32_t val)
--
writeReg :: Ident -> CExpr -> CExpr -> CExpr
writeReg funcName idx val = CCall (CVar funcName undefNode) [idx, val] undefNode

------------------------------------------------------------------------

-- Interface contract:
--
--  uint32_t instr_rs1(void *instr);
--
instrRS1 :: CExpr -> Ident -> CExpr
instrRS1 instr funcName = CCall (CVar funcName undefNode) [instr] undefNode

-- Interface contract:
--
--  uint32_t instr_rs2(void *instr);
--
instrRS2 :: CExpr -> Ident -> CExpr
instrRS2 instr funcName = CCall (CVar funcName undefNode) [instr] undefNode

-- Interface contract:
--
--  uint32_t instr_rd(void *instr);
--
instrRD :: CExpr -> Ident -> CExpr
instrRD instr funcName = CCall (CVar funcName undefNode) [instr] undefNode
