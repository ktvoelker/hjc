
module Compiler where

import CoreSyn as Hs
import HscTypes as Hsc
import Module as HsMod
import Name as HsName
import Var as HsVar

import Ast as Js

compileModule :: Hsc.ModGuts -> Js.Module
compileModule mg =
  Module (hsModuleName $ Hsc.mg_module mg)
  $ concatMap (map (uncurry compileBinding) . flattenBinding) $ Hsc.mg_binds mg

flattenBinding :: Hs.CoreBind -> [(Hs.CoreBndr, Hs.Expr Hs.CoreBndr)]
flattenBinding (NonRec b e) = [(b, e)]
flattenBinding (Rec bs) = bs

hsModuleName :: HsMod.Module -> String
hsModuleName = HsMod.moduleNameString . HsMod.moduleName

hsName :: Hs.CoreBndr -> (Maybe String, String)
hsName bndr = 
  ( fmap hsModuleName $ HsName.nameModule_maybe name
  , HsName.occNameString $ HsName.nameOccName name)
  where
    name = HsVar.varName bndr

compileBinding :: Hs.CoreBndr -> Hs.Expr Hs.CoreBndr -> Js.Binding
compileBinding bndr expr = (snd $ hsName bndr, compileExpr expr)

compileExpr :: Hs.Expr Hs.CoreBndr -> Js.Expr
compileExpr = const $ Literal $ LitStr "unimplemented"

