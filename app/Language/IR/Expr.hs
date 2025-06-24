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
  | Int Integer
  | Not Expr
  | Equal Expr Expr
  | Bool Bool
  | If Expr Expr Expr
  | Print Expr
  deriving (Eq, Show)
