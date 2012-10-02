
module TyCons (compileTyCons) where

import qualified CoreSyn as Hs
import qualified DataCon as HsDc
import qualified Name as HsName
import qualified TyCon as HsTc
import qualified Unique as HsU
import qualified Var as HsVar

import Ast
import Util

compileTyCons :: [HsTc.TyCon] -> [Binding]
compileTyCons = concatMap compileTyCon

compileTyCon :: HsTc.TyCon -> [Binding]
compileTyCon tc | HsTc.isAlgTyCon tc = case HsTc.algTyConRhs tc of
  HsTc.AbstractTyCon _ -> []
  HsTc.DataFamilyTyCon -> []
  HsTc.NewTyCon { HsTc.data_con = dc } -> compileNewDataCon dc
  HsTc.DataTyCon { HsTc.data_cons = ds } -> concatMap compileDataCon ds

compileNewDataCon :: HsDc.DataCon -> [Binding]
compileNewDataCon dc =
    Binding (compileName $ HsDc.dataConName dc) (ENative "I") False
  : []

compileDataCon :: HsDc.DataCon -> [Binding]
compileDataCon dc =
  let name = compileName $ HsDc.dataConName dc in
  Binding
    name
    (Call (ENative "C")
      [ name
      , Literal $ LitInteger $ toInteger $ length $ HsDc.dataConRepArgTys dc
      , Array []
      ])
    False
  : []
 
