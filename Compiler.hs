
module Compiler where

import qualified CoreSyn as Hs
import qualified FastString as HsFs
import qualified HscTypes as Hsc
import qualified Literal as HsLit
import qualified Module as HsMod
import qualified Name as HsName
import qualified Unique as HsU
import qualified Var as HsVar

import Ast

compileModule :: Hsc.ModGuts -> Module
compileModule mg =
  Module (hsModuleName $ Hsc.mg_module mg)
  $ concatMap (map (uncurry compileBinding) . flattenBinding) $ Hsc.mg_binds mg

flattenBinding :: Hs.CoreBind -> [(Hs.CoreBndr, Hs.Expr Hs.CoreBndr)]
flattenBinding (Hs.NonRec b e) = [(b, e)]
flattenBinding (Hs.Rec bs) = bs

hsModuleName :: HsMod.Module -> String
hsModuleName = HsMod.moduleNameString . HsMod.moduleName

hsName :: Hs.CoreBndr -> (Maybe String, String)
hsName bndr = 
  ( fmap hsModuleName $ HsName.nameModule_maybe name
  , HsName.occNameString $ HsName.nameOccName name)
  where
    name = HsVar.varName bndr

compileBinding :: Hs.CoreBndr -> Hs.Expr Hs.CoreBndr -> Binding
compileBinding bndr expr = (snd $ hsName bndr, compileExpr expr)

compileExpr :: Hs.Expr Hs.CoreBndr -> Expr
compileExpr e = case e of
  Hs.Var name -> Use $ case hsName name of
    (Nothing, name) -> LocalName name
    (Just modName, name) -> GlobalName modName name
  Hs.Lit (HsLit.MachStr xs) -> Call (ENative "S") [Literal $ LitStr $ HsFs.unpackFS xs]
  Hs.Lit lit -> Literal $ case lit of
    HsLit.MachChar c -> LitChar c
    HsLit.MachNullAddr -> undefined
    HsLit.MachInt n -> LitNum $ fromIntegral n
    HsLit.MachInt64 n -> LitNum $ fromIntegral n
    HsLit.MachWord n -> LitNum $ fromIntegral n
    HsLit.MachWord64 n -> LitNum $ fromIntegral n
    HsLit.MachFloat n -> LitNum $ fromRational n
    HsLit.MachDouble n -> LitNum $ fromRational n
    HsLit.MachLabel _ _ _ -> undefined
    HsLit.LitInteger _ _ -> undefined
  Hs.App e a -> Call (compileExpr e) [compileExpr a]
  Hs.Lam id e -> Func [snd $ hsName id] [Return $ compileExpr e]
  Hs.Let bs e -> let (bNames, bVals) = unzip $ flattenBinding bs in
    Call (Func (map (snd . hsName) bNames)
      [Return $ compileExpr e]) (map compileExpr bVals)
  Hs.Case scru name _ alts -> compileCase scru name alts
  Hs.Type _ -> ENative "T"

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
  StrictEq (Index (Use $ LocalName id) (Literal $ LitStr "co"))
    $ Literal $ LitNum $ fromIntegral $ HsU.getKey $ HsU.getUnique dataCon
testAlt id (Hs.LitAlt lit) = case lit of
  HsLit.MachChar c ->
    StrictEq (Use $ LocalName id) $ Literal $ LitChar c
  HsLit.MachStr xs ->
    undefined
  HsLit.MachNullAddr ->
    undefined
  HsLit.MachInt n ->
    StrictEq (Use $ LocalName id) $ Literal $ LitNum $ fromIntegral n
  HsLit.MachInt64 n ->
    StrictEq (Use $ LocalName id) $ Literal $ LitNum $ fromIntegral n
  HsLit.MachWord n ->
    StrictEq (Use $ LocalName id) $ Literal $ LitNum $ fromIntegral n
  HsLit.MachWord64 n ->
    StrictEq (Use $ LocalName id) $ Literal $ LitNum $ fromIntegral n
  HsLit.MachFloat n ->
    StrictEq (Use $ LocalName id) $ Literal $ LitNum $ fromRational n
  HsLit.MachDouble n ->
    StrictEq (Use $ LocalName id) $ Literal $ LitNum $ fromRational n
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
    . Literal . LitNum . fromIntegral

