
module Compiler (compileModule) where

import qualified CoreSyn as Hs
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

compileModule :: Maybe String -> Hsc.ModGuts -> Module
compileModule maybeMain mg =
  Module
  { m_name     = name
  , m_bindings = bindings
  , m_main     = fmap b_lhs $ find b_main bindings
  }
  where
    bindings = concatMap (map (uncurry $ compileBinding maybeMain) . flattenBinding)
      (Hsc.mg_binds mg) ++ compileTyCons (Hsc.mg_tcs mg)
    name = hsModuleName $ Hsc.mg_module mg

flattenBinding :: Hs.CoreBind -> [(Hs.CoreBndr, Hs.Expr Hs.CoreBndr)]
flattenBinding (Hs.NonRec b e) = [(b, e)]
flattenBinding (Hs.Rec bs) = bs

compileBinding :: Maybe String -> Hs.CoreBndr -> Hs.Expr Hs.CoreBndr -> Binding
compileBinding maybeMain bndr expr = Binding (unique bndr) (compileExpr expr) isMain
  where
    isMain = case (maybeMain, hsName bndr) of
      (Nothing, _) -> False
      (Just _, (Nothing, _)) -> False
      (Just main, (Just modName, varName)) -> main == modName ++ "." ++ varName

compileExpr :: Hs.Expr Hs.CoreBndr -> Expr
compileExpr e = case e of
  Hs.Var name -> Use $ case hsName name of
    (Nothing, name) -> LocalName name
    (Just modName, name) -> GlobalName modName name
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
  StrictEq (Index (Use $ LocalName id) (Literal $ LitStr "co")) $ unique dataCon
testAlt id (Hs.LitAlt lit) = case lit of
  HsLit.MachChar c ->
    StrictEq (Use $ LocalName id) $ Literal $ LitChar c
  HsLit.MachStr xs ->
    undefined
  HsLit.MachNullAddr ->
    undefined
  HsLit.MachInt n ->
    StrictEq (Use $ LocalName id) $ Literal $ LitInteger n
  HsLit.MachInt64 n ->
    StrictEq (Use $ LocalName id) $ Literal $ LitInteger n
  HsLit.MachWord n ->
    StrictEq (Use $ LocalName id) $ Literal $ LitInteger n
  HsLit.MachWord64 n ->
    StrictEq (Use $ LocalName id) $ Literal $ LitInteger n
  HsLit.MachFloat n ->
    StrictEq (Use $ LocalName id) $ Literal $ LitDouble $ fromRational n
  HsLit.MachDouble n ->
    StrictEq (Use $ LocalName id) $ Literal $ LitDouble $ fromRational n
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
  Index (Index (Use $ LocalName id) (Literal $ LitStr "xs"))
  . Literal . LitInteger . toInteger

