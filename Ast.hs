
module Ast where

data Module =
  Module
  { m_name     :: Id
  , m_bindings :: [Binding]
  }

data Binding =
  Binding
  { b_lhs  :: Expr
  , b_rhs  :: Expr
  }

type Id = String

type Block = [Stmt]

data Expr =
    Func [Id] Block
  | Call Expr [Expr]
  | Use Id
  | Index Expr Expr
  | Literal LitVal
  | Array [Expr]
  | Object [(Id, Expr)]
  | StrictEq Expr Expr
  | If Expr Expr Expr
  | ENative String

data LitVal =
    LitChar Char
  | LitInteger Integer
  | LitDouble Double
  | LitStr String
  | LitNull
  | LitUndef

data Stmt =
    Return Expr
  | Assign Expr Expr
  | Exec Expr
  | SNative String
  | Var Id Expr

