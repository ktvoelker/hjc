
module Output where

import Ast

writeModules :: FilePath -> [Module] -> IO ()
writeModules name = writeFile name . show

modulesRootId = "M"
mainName = GlobalName "Main" "main"

natives = ""

makeProgram :: [Module] -> Expr
makeProgram ms = Call (Func [] $ ss ++ [Return $ Call (Use mainName) []]) []
  where
    ss =
        (Var modulesRootId $ Object [])
      : Native natives
      : concatMap (\m -> map (makeModuleBinding $ m_name m) $ m_bindings m) ms

makeModuleBinding :: Id -> Binding -> Stmt
makeModuleBinding modName (bindName, expr) =
  Assign (Use $ GlobalName modName bindName) expr

instance Show Module where
  showsPrec _ _ = undefined
  showList = shows . makeProgram

instance Show Name where
  showsPrec _ (GlobalName modName bindName) =
      (modulesRootId ++)
    . ("['" ++)
    . (modName ++)
    . ("']['" ++)
    . (bindName ++)
    . ("']" ++)
  showsPrec _ (LocalName name) =
      ("_" ++)
    . (name ++)

instance Show Expr where
  showsPrec p (Func ps ss) =
      ("function(" ++)
    . showsWithCommas p ps
    . ("){" ++)
    . showsWithSemis p ss
    . ("}" ++)
  showsPrec p (Call e as) =
      ("(" ++)
    . showsPrec p e
    . (")(" ++)
    . showsWithCommas p as
    . (")" ++)
  showsPrec p (Use name) = showsPrec p name
  showsPrec p (Index x i) =
      ("(" ++)
    . showsPrec p x
    . (")[" ++)
    . showsPrec p i
    . ("]" ++)
  showsPrec p (Literal lv) = showsPrec p lv
  showsPrec p (Array es) =
      ("[" ++)
    . showsWithCommas p es
    . ("]" ++)
  showsPrec p (Object es) =
      ("{" ++)
    . showsWithCommas p (map (uncurry Pair) es)
    . ("}" ++)

data Pair = Pair Id Expr

instance Show Pair where
  showsPrec p (Pair n e) =
      ("'" ++)
    . (n ++)
    . ("':" ++)
    . showsPrec p e

instance Show LitVal where
  showsPrec p (LitNum d) = showsPrec p d
  showsPrec p (LitStr xs) = showsPrec p xs

showsWithCommas _ [] = id
showsWithCommas p (x : xs) =
    showsPrec p x
  . foldr (.) id (map (\x -> ("," ++) . showsPrec p x) xs)

showsWithSemis p =
    foldr (.) id
  . map (\x -> showsPrec p x . (";" ++))

instance Show Stmt where
  showsPrec p (Return e) =
      ("return " ++)
    . showsPrec p e
  showsPrec p (Var id e) =
      ("var " ++)
    . showsPrec p (Assign (Use $ LocalName id) e)
  showsPrec p (Assign lhs rhs) =
      showsPrec p lhs
    . ("=" ++)
    . showsPrec p rhs
  showsPrec p (Exec e) = showsPrec p e
  showsPrec p (Native xs) = (xs ++)

