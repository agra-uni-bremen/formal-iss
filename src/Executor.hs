module Executor (makeExecutor) where

import Language.C

instrArg :: Ident -> CDecl
instrArg instrIdent = CDecl
    [(CTypeSpec $ CVoidType undefNode)]
    [((Just instrArg'), Nothing, Nothing)]
    undefNode
  where
    instrArg' :: CDeclr
    instrArg' = CDeclr (Just instrIdent) [CPtrDeclr [] undefNode] Nothing [] undefNode

------------------------------------------------------------------------

noReturn :: CDeclSpec
noReturn = CTypeSpec $ CVoidType undefNode

mkFuncDeclr :: Ident -> [CDecl] -> CDeclr
mkFuncDeclr ident args = CDeclr (Just ident) [mkFuncDeclr' args] Nothing [] undefNode
    where
        mkFuncDeclr' :: [CDecl] -> CDerivedDeclr
        mkFuncDeclr' args' = CFunDeclr (Right (args', False)) [] undefNode

makeExecutor :: Ident -> Ident -> CStat -> CFunDef
makeExecutor funcIdent instrIdent statement = CFunDef
                                               [noReturn]
                                               (mkFuncDeclr funcIdent [instrArg instrIdent])
                                               []
                                               statement
                                               undefNode
