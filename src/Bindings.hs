module Bindings (Bindings, mkBindings, getIdent) where

import Language.C
import Data.Maybe (fromJust)
import qualified Data.Map as M

-- Store Ident here, not Name
type Bindings = M.Map String Ident

mkBinding :: String -> Name -> (String, Ident)
mkBinding s name = (s, mkIdent nopos s name)

-- TODO: Fold
mkBindings :: [Name] -> Bindings
mkBindings names = M.fromList $ snd $ foldl (\(n, acc) s -> (tail n, (mkBinding s $ head n) : acc)) (names, []) ids
  where
    ids = [
            "read_register",  -- Read register interface function
            "write_register", -- Write register interface function

            "instr_rs1",      -- Interface function for RS1 access
            "instr_rs2",      -- Interface function for RS2 access
            "instr_rd"        -- Interface function for RD access
          ]

getIdent :: String -> Bindings -> Ident
getIdent name bindings = fromJust (M.lookup name bindings)
