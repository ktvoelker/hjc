
module Output where

import Data.Maybe

import Ast

defaultNatives = "natives.js"

writeModules :: Maybe FilePath -> FilePath -> [Module] -> IO ()
writeModules maybeNatives name ms =
  readFile (fromMaybe defaultNatives maybeNatives)
  >>= writeFile name . flip showModules ms

modulesRootId = "M"

makeProgram :: String -> [Module] -> Expr
makeProgram natives ms = Call (Func [] $ ss ++ mc ++ rt) []
  where
    ss =
        Var modulesRootId (Object [])
      : SNative natives
      : concatMap (\m -> map (makeModuleBinding $ m_name m) $ m_bindings m) ms
    mc = map (\main -> Exec (Call (ENative "R") [Call main []]))
      $ take 1 $ catMaybes $ map m_main ms
    rt = [Return $ Literal LitUndef]

makeModuleBinding :: Id -> Binding -> Stmt
makeModuleBinding modName (Binding bindName expr _) =
  Assign (Index (Index (ENative "M") (Literal $ LitStr modName)) bindName) expr

showModules :: String -> [Module] -> String
showModules natives = show . makeProgram natives

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
      -- TODO fix the un-perfect badness
    . showsWithCommas p (map ("_" ++) ps)
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
  showsPrec p (StrictEq a b) =
      ("((" ++)
    . (showsPrec p a)
    . (") === (" ++)
    . (showsPrec p b)
    . ("))" ++)
  showsPrec p (If c t f) =
      ("((" ++)
    . (showsPrec p c)
    . (") ? (" ++)
    . (showsPrec p t)
    . (") : (" ++)
    . (showsPrec p f)
    . (")" ++)
  showsPrec p (ENative xs) =
      ("(" ++)
    . (xs ++)
    . (")" ++)

data Pair = Pair Id Expr

instance Show Pair where
  showsPrec p (Pair n e) =
      ("'" ++)
    . (n ++)
    . ("':" ++)
    . showsPrec p e

instance Show LitVal where
  showsPrec p (LitChar c) = showsPrec p c
  showsPrec p (LitInteger n) = showsPrec p n
  showsPrec p (LitDouble d) = showsPrec p d
  showsPrec p (LitStr xs) = showsPrec p xs
  showsPrec _ LitNull = ("null" ++)
  showsPrec _ LitUndef = ("undefined" ++)

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
    . (id ++)
    . ("=" ++)
    . showsPrec p e
  showsPrec p (Assign lhs rhs) =
      showsPrec p lhs
    . ("=" ++)
    . showsPrec p rhs
  showsPrec p (Exec e) = showsPrec p e
  showsPrec p (SNative xs) = (xs ++)

