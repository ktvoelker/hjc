
module Util where

import qualified CoreSyn as Hs
import qualified Module as HsMod
import qualified Name as HsName
import qualified Unique as HsU
import qualified Var as HsVar

import Ast

hsModuleName :: HsMod.Module -> String
hsModuleName = HsMod.moduleNameString . HsMod.moduleName

hsName :: Hs.CoreBndr -> (Maybe String, String)
hsName bndr = 
  ( fmap hsModuleName $ HsName.nameModule_maybe name
  , HsName.occNameString $ HsName.nameOccName name)
  where
    name = HsVar.varName bndr

unique :: HsU.Uniquable u => u -> Expr
unique = Literal . LitNum . fromIntegral . HsU.getKey . HsU.getUnique

