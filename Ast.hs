
module Ast where

data Module = Module Id [Binding]

type Binding = (Id, Expr)

type Id = String

type Block = [Stmt]

data Name =
    GlobalName Id Id
  | LocalName Id
  | NativeName Id Id

data Expr =
    Func [Id] Block
  | Call Expr [Expr]
  | Use Name
  | Index Expr (Either Id Expr)
  | Literal LitVal
  | Array [Expr]
  | Object [(Id, Expr)]

data LitVal =
    LitNum Double
  | LitStr String

data Stmt =
    Return Expr

