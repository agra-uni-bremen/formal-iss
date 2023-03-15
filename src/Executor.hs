module Executor (instrArg, pcArg, makeExecutor) where

import Bindings
import Interface qualified as IF
import Language.C

instrArg :: Ident -> CDecl
instrArg instrIdent =
    CDecl
        [CTypeSpec $ CVoidType undefNode]
        [(Just instrArg', Nothing, Nothing)]
        undefNode
  where
    instrArg' :: CDeclr
    instrArg' = CDeclr (Just instrIdent) [CPtrDeclr [] undefNode] Nothing [] undefNode

pcArg :: Bindings -> Ident -> CDecl
pcArg binds pcIdent =
    CDecl
        [IF.uint32 binds]
        [(Just pcArg', Nothing, Nothing)]
        undefNode
  where
    pcArg' :: CDeclr
    pcArg' = CDeclr (Just pcIdent) [] Nothing [] undefNode

------------------------------------------------------------------------

mkFuncDeclr :: Ident -> [CDecl] -> CDeclr
mkFuncDeclr ident args = CDeclr (Just ident) [mkFuncDeclr' args] Nothing [] undefNode
  where
    mkFuncDeclr' :: [CDecl] -> CDerivedDeclr
    mkFuncDeclr' args' = CFunDeclr (Right (args', False)) [] undefNode

makeExecutor :: Ident -> [CDecl] -> CStat -> CFunDef
makeExecutor funcIdent args statement =
    CFunDef
        [CStorageSpec (CStatic undefNode), CFunSpec (CInlineQual undefNode), voidReturn]
        (mkFuncDeclr funcIdent args)
        []
        statement
        undefNode
  where
    voidReturn :: CDeclSpec
    voidReturn = CTypeSpec $ CVoidType undefNode
