module Language.IR.Expr
  ( Expr (..),
  )
where

data Expr
  = Unit
  | Call String
  | VarExpr String
  | Block [Expr] Expr
  | Set String Expr
  | Str String
  | List [Expr]
  | Get Expr Int
  | Int Int
  deriving (Show)
