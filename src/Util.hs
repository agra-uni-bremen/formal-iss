module Util where

import Bindings
import Data.Char
import Language.C

-- Convert an entire string to lowercase.
foldcase :: String -> String
foldcase = map toLower

-- Helper function to perform type conversions via casts.
castTo :: CDeclSpec -> CExpr -> CExpr
castTo ty expr = CCast (CDecl [ty] [] undefNode) expr undefNode

-- Create a 'CDeclSpec' for a type defined through a typedef elsewhere.
typedef :: String -> Bindings -> CDeclSpec
typedef s b = CTypeSpec (CTypeDef typeIdent undefNode)
  where
    typeIdent = getIdent s b
