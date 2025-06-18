module Language.IR.Expr
  ( Expr (..),
  )
where

data Expr
  = Unit
  | Call String
  | VarExpr String
