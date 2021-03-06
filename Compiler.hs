
module Compiler (compileModule) where

import qualified CoreSyn as Hs
import qualified DataCon as HsDc
import qualified FastString as HsFs
import qualified HscTypes as Hsc
import qualified Literal as HsLit
import qualified Module as HsMod
import qualified Name as HsName
import qualified Var as HsVar

import Data.List

import Ast
import TyCons
import Util

compileModule :: Hsc.ModGuts -> Module
compileModule mg =
  Module
  { m_name     = name
  , m_bindings = bindings
  }
  where
    bindings = concatMap (map (uncurry compileBinding) . flattenBinding)
      (Hsc.mg_binds mg) ++ compileTyCons (Hsc.mg_tcs mg)
    name = hsModuleName $ Hsc.mg_module mg

flattenBinding :: Hs.CoreBind -> [(Hs.CoreBndr, Hs.Expr Hs.CoreBndr)]
flattenBinding (Hs.NonRec b e) = [(b, e)]
flattenBinding (Hs.Rec bs) = bs

compileBinding :: Hs.CoreBndr -> Hs.Expr Hs.CoreBndr -> Binding
compileBinding bndr expr = Binding (compileName bndr) (compileExpr expr)

compileExpr :: Hs.Expr Hs.CoreBndr -> Expr
compileExpr e = case e of
  Hs.Var name -> compileName name
  Hs.Lit lit -> Literal $ case lit of
    HsLit.MachChar c -> LitChar c
    HsLit.MachNullAddr -> undefined
    HsLit.MachStr xs -> LitStr $ HsFs.unpackFS xs
    HsLit.MachInt n -> LitInteger n
    HsLit.MachInt64 n -> LitInteger n
    HsLit.MachWord n -> LitInteger n
    HsLit.MachWord64 n -> LitInteger n
    HsLit.MachFloat n -> LitDouble $ fromRational n
    HsLit.MachDouble n -> LitDouble $ fromRational n
    HsLit.MachLabel _ _ _ -> undefined
    HsLit.LitInteger _ _ -> undefined
  Hs.App e a -> Object [("ap", compileExpr e), ("ar", compileExpr a)]
  Hs.Lam id e -> Func [snd $ hsName id] [Return $ compileExpr e]
  Hs.Let bs e -> let (bNames, bVals) = unzip $ flattenBinding bs in
    Call (Func (map (snd . hsName) bNames)
      [Return $ compileExpr e]) (map compileExpr bVals)
  Hs.Case scru name _ alts -> compileCase scru name alts
  Hs.Type _ -> ENative "T"
  Hs.Cast e _ -> compileExpr e
  _ -> undefined

compileCase :: Hs.Expr Hs.CoreBndr -> Hs.CoreBndr -> [Hs.Alt Hs.CoreBndr] -> Expr
compileCase scru name alts =
  let id = snd $ hsName name in
  Call (Func [id] [Return $ compileAlts id alts])
    [Call (ENative "F") [compileExpr scru]]

compileAlts :: Id -> [Hs.Alt Hs.CoreBndr] -> Expr
compileAlts id =
  foldr (\alt@(con, _, _) -> If (testAlt id con) (chooseAlt id alt))
    (Call (ENative "E") [Literal $ LitStr "inexhaustive alternatives"])

testAlt :: Id -> Hs.AltCon -> Expr
testAlt _ Hs.DEFAULT = ENative "true"
testAlt id (Hs.DataAlt dataCon) =
  StrictEq (Index (Use id) (Literal $ LitStr "co"))
  $ compileName $ HsDc.dataConName dataCon
testAlt id (Hs.LitAlt lit) = case lit of
  HsLit.MachChar c ->
    StrictEq (Use id) $ Literal $ LitChar c
  HsLit.MachStr xs ->
    undefined
  HsLit.MachNullAddr ->
    undefined
  HsLit.MachInt n ->
    StrictEq (Use id) $ Literal $ LitInteger n
  HsLit.MachInt64 n ->
    StrictEq (Use id) $ Literal $ LitInteger n
  HsLit.MachWord n ->
    StrictEq (Use id) $ Literal $ LitInteger n
  HsLit.MachWord64 n ->
    StrictEq (Use id) $ Literal $ LitInteger n
  HsLit.MachFloat n ->
    StrictEq (Use id) $ Literal $ LitDouble $ fromRational n
  HsLit.MachDouble n ->
    StrictEq (Use id) $ Literal $ LitDouble $ fromRational n
  HsLit.MachLabel _ _ _ ->
    undefined
  HsLit.LitInteger _ _ ->
    undefined

chooseAlt :: Id -> Hs.Alt Hs.CoreBndr -> Expr
chooseAlt _ (_, [], e) = compileExpr e
chooseAlt _ (Hs.DEFAULT, _, _) = undefined
chooseAlt _ (Hs.LitAlt _, _, _) = undefined
chooseAlt id (Hs.DataAlt dataCon, bs, e) =
  -- TODO: we could use JS apply directly on the internal array of the algebraic value
  Call (Func (map (snd . hsName) bs) [Return $ compileExpr e])
    $ map (extractAlgIndex id) [0 .. length bs - 1]

extractAlgIndex :: Id -> Int -> Expr
extractAlgIndex id =
  Index (Index (Use id) (Literal $ LitStr "xs"))
  . Literal . LitInteger . toInteger

