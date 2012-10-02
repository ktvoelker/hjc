
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
makeProgram natives ms =
  Call (Func [] $ prel ++ mods ++ nats ++ binds ++ main ++ ret) []
  where
    prel = [Var modulesRootId (Object [])]
    mods = map (makeModuleMap . m_name) ms
    nats = [SNative natives]
    binds = concatMap (map makeAssignment . m_bindings) ms
    main = map (Exec . Call (ENative "R") . (: []))
      $ take 1 $ catMaybes $ map m_main ms
    ret = [Return $ Literal LitUndef]

makeModuleMap :: String -> Stmt
makeModuleMap = flip Assign (Object []) . Index (ENative "M") . Literal . LitStr

makeAssignment :: Binding -> Stmt
makeAssignment (Binding lhs rhs _) = Assign lhs rhs

showModules :: String -> [Module] -> String
showModules natives mods = show (makeProgram natives mods) ++ ";\n"

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
  showsPrec _ (Use id) =
      ("(_" ++)
    . (id ++)
    . (")" ++)
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
  . map (\x -> showsPrec p x . (";\n" ++))

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

