-- This module defines the C API required to be implemented for the ISS.
module Interface where

import Bindings
import Language.C

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
--  uint32_t instr_rs1(void *instr);
--
instrRS1 :: CExpr -> Bindings -> CExpr
instrRS1 instr binds = CCall (CVar funcIdent undefNode) [instr] undefNode
  where
    funcIdent = getIdent "instr_rs1" binds

-- Interface contract:
--
--  uint32_t instr_rs2(void *instr);
--
instrRS2 :: CExpr -> Bindings -> CExpr
instrRS2 instr binds = CCall (CVar funcIdent undefNode) [instr] undefNode
  where
    funcIdent = getIdent "instr_rs2" binds

-- Interface contract:
--
--  uint32_t instr_rd(void *instr);
--
instrRD :: CExpr -> Bindings -> CExpr
instrRD instr binds = CCall (CVar funcIdent undefNode) [instr] undefNode
  where
    funcIdent = getIdent "instr_rd" binds
