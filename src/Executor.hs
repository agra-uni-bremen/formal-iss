module Executor (instrIdent, makeExecutor) where

import Language.C
import Interface (names)

instrIdent :: Ident
instrIdent = mkIdent nopos "instr" (names !! 5)

instrArg :: CDecl
instrArg = CDecl
    [(CTypeSpec $ CVoidType (mkNodeInfoOnlyPos nopos))]
    [((Just instrArg'), Nothing, Nothing)]
    (mkNodeInfoOnlyPos nopos)
  where
    instrArg' :: CDeclr
    instrArg' = CDeclr (Just instrIdent) [CPtrDeclr [] (mkNodeInfoOnlyPos nopos)] Nothing [] (mkNodeInfoOnlyPos nopos)

-- regIndent :: Ident
-- regIndent = mkIdent nopos "regFile" (names !! 6)

------------------------------------------------------------------------

noReturn :: CDeclSpec
noReturn = CTypeSpec $ CVoidType (mkNodeInfoOnlyPos nopos)

mkFuncDeclr :: Ident -> [CDecl] -> CDeclr
mkFuncDeclr ident args = CDeclr (Just ident) [mkFuncDeclr' args] Nothing [] (mkNodeInfoOnlyPos nopos)
    where
        mkFuncDeclr' :: [CDecl] -> CDerivedDeclr
        mkFuncDeclr' args' = CFunDeclr (Right (args', False)) [] (mkNodeInfoOnlyPos nopos)

makeExecutor :: Ident -> CStat -> CFunDef
makeExecutor ident statement = CFunDef
                                    [noReturn]
                                    (mkFuncDeclr ident [instrArg])
                                    []
                                    statement
                                    (mkNodeInfoOnlyPos nopos)
