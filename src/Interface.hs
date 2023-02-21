-- This module defines the C API required to be implemented for the ISS.
module Interface where

import Language.C

names :: [Name]
names = newNameSupply

------------------------------------------------------------------------

-- Interface contract:
--
--  uint32_t read_register(unsigned idx);
--
readReg :: CExpr -> CExpr
readReg idx = CCall (CVar funcName $ mkNodeInfoOnlyPos nopos) [idx] $ mkNodeInfoOnlyPos nopos
  where
    funcName :: Ident
    funcName = mkIdent nopos "read_register" $ names !! 0

-- Interface contract:
--
--  void write_register(unsigned idx, uint32_t val)
--
writeReg :: CExpr -> CExpr -> CExpr
writeReg idx val = CCall (CVar funcName $ mkNodeInfoOnlyPos nopos) [idx, val] $ mkNodeInfoOnlyPos nopos
  where
    funcName :: Ident
    funcName = mkIdent nopos "write_register" $ names !! 1

------------------------------------------------------------------------

-- Interface contract:
--
--  uint32_t instr_rs1(void *instr);
--
instrRS1 :: CExpr -> CExpr
instrRS1 instr = CCall (CVar funcName $ mkNodeInfoOnlyPos nopos) [instr] $ mkNodeInfoOnlyPos nopos
  where
    funcName :: Ident
    funcName = mkIdent nopos "instr_rs1" $ names !! 2

-- Interface contract:
--
--  uint32_t instr_rs2(void *instr);
--
instrRS2 :: CExpr -> CExpr
instrRS2 instr = CCall (CVar funcName $ mkNodeInfoOnlyPos nopos) [instr] $ mkNodeInfoOnlyPos nopos
  where
    funcName :: Ident
    funcName = mkIdent nopos "instr_rs2" $ names !! 3

-- Interface contract:
--
--  uint32_t instr_rd(void *instr);
--
instrRD :: CExpr -> CExpr
instrRD instr = CCall (CVar funcName $ mkNodeInfoOnlyPos nopos) [instr] $ mkNodeInfoOnlyPos nopos
  where
    funcName :: Ident
    funcName = mkIdent nopos "instr_rd" $ names !! 4
