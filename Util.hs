
{-# LANGUAGE TypeSynonymInstances #-}
module Util where

import qualified CoreSyn as Hs
import qualified Module as HsMod
import qualified Name as HsName
import qualified Unique as HsU
import qualified Var as HsVar

import Ast

class GetName a where
  hsName :: a -> (Maybe String, String)

instance GetName Hs.CoreBndr where
  hsName = hsName . HsVar.varName

instance GetName HsName.Name where
  hsName name =
    ( fmap hsModuleName $ HsName.nameModule_maybe name
    , HsName.occNameString $ HsName.nameOccName name
    )

hsModuleName :: HsMod.Module -> String
hsModuleName = HsMod.moduleNameString . HsMod.moduleName

unique :: HsU.Uniquable u => u -> Expr
unique = Literal . LitInteger . toInteger . HsU.getKey . HsU.getUnique

compileName :: (GetName a) => a -> Expr
compileName bndr =
  let (maybeModName, varName) = hsName bndr in
  case maybeModName of
    Nothing -> Use varName
    Just modName ->
      Index (Index (ENative "M") (Literal $ LitStr modName))
      $ Literal $ LitStr varName

