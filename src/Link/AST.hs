module Link.AST where

newtype AST
  = AST Expr

data Expr
  = Unit
  | Int Int
  | CallExpr Call

data Call
  = Call String Expr
