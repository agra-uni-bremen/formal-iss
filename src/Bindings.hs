module Bindings (Bindings, mkBindings, getIdent) where

import Data.Map qualified as M
import Data.Maybe (fromJust)
import Language.C

{- FOURMOLU_DISABLE -}
-- Statically known function names required to be provided as an Interface.
funcNames :: [String]
funcNames =
    [ "uint8_t"        -- Typedef from stdint.h
    , "uint32_t"       -- Typedef from stdint.h

    , "read_register"  -- Read register interface function
    , "write_register" -- Write register interface function

    , "instr_rs1"      -- Interface function for RS1 access
    , "instr_rs2"      -- Interface function for RS2 access
    , "instr_rd"       -- Interface function for RD access
    ]
{- FOURMOLU_ENABLE -}

-- Store Ident here, not Name
type Bindings = M.Map String Ident

mkBinding :: String -> Name -> (String, Ident)
mkBinding s name = (s, mkIdent nopos s name)

mkBindings :: [Name] -> (Bindings, [Name])
mkBindings names = (M.fromList $ pairs, newNs)
  where
    (newNs, pairs) = foldl fn (names, []) funcNames
    fn (n, acc) s = (tail n, mkBinding s (head n) : acc)

getIdent :: String -> Bindings -> Ident
getIdent name bindings = fromJust (M.lookup name bindings)
