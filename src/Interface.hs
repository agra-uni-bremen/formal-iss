-- This module defines the C API required to be implemented for the ISS.
module Interface (readRegFunc, writeRegFunc) where

import Language.C

names :: [Name]
names = newNameSupply

------------------------------------------------------------------------

-- Interface contract:
--
--  uint32_t read_register(unsigned idx);
--
readRegFunc :: CExpr -> CExpr
readRegFunc idx = CCall (CVar funcName $ mkNodeInfoOnlyPos nopos) [idx] $ mkNodeInfoOnlyPos nopos
  where
    funcName :: Ident
    funcName = mkIdent nopos "read_register" $ names !! 0

-- Interface contract:
--
--  void write_register(unsigned idx, uint32_t val)
--
writeRegFunc :: CExpr -> CExpr -> CExpr
writeRegFunc idx val = CCall (CVar funcName $ mkNodeInfoOnlyPos nopos) [idx, val] $ mkNodeInfoOnlyPos nopos
  where
    funcName :: Ident
    funcName = mkIdent nopos "write_register" $ names !! 1
