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
  deriving (Show)
