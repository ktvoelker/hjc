
module Compiler where

import qualified CoreSyn as Hs
import qualified FastString as HsFs
import qualified HscTypes as Hsc
import qualified Literal as HsLit
import qualified Module as HsMod
import qualified Name as HsName
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
  Hs.Lit lit -> Literal $ case lit of
    HsLit.MachChar c -> LitChar c
    HsLit.MachStr xs -> LitStr $ HsFs.unpackFS xs
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
  Hs.Case scru id ty alts -> compileCase scru id ty alts
  Hs.Type _ -> Type

compileCase scru id ty alts = undefined

