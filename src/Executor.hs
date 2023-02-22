module Executor (instrIdent, makeExecutor) where

import Language.C
import Interface (names)

instrIdent :: Ident
instrIdent = mkIdent nopos "instr" (names !! 5)

instrArg :: CDecl
instrArg = CDecl
    [(CTypeSpec $ CVoidType undefNode)]
    [((Just instrArg'), Nothing, Nothing)]
    undefNode
  where
    instrArg' :: CDeclr
    instrArg' = CDeclr (Just instrIdent) [CPtrDeclr [] undefNode] Nothing [] undefNode

-- regIndent :: Ident
-- regIndent = mkIdent nopos "regFile" (names !! 6)

------------------------------------------------------------------------

noReturn :: CDeclSpec
noReturn = CTypeSpec $ CVoidType undefNode

mkFuncDeclr :: Ident -> [CDecl] -> CDeclr
mkFuncDeclr ident args = CDeclr (Just ident) [mkFuncDeclr' args] Nothing [] undefNode
    where
        mkFuncDeclr' :: [CDecl] -> CDerivedDeclr
        mkFuncDeclr' args' = CFunDeclr (Right (args', False)) [] undefNode

makeExecutor :: Ident -> CStat -> CFunDef
makeExecutor ident statement = CFunDef
                                    [noReturn]
                                    (mkFuncDeclr ident [instrArg])
                                    []
                                    statement
                                    undefNode
