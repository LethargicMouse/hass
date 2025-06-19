module Language.Trust.Expr
  ( Block (..),
    Expr (..),
    Call (..),
    Var (..),
  )
where

import Language.View (View)

data Expr
  = Unit
  | CallExpr Call
  | VarExpr Var

newtype Block
  = Block Expr

data Call
  = Call View String [Expr]

data Var
  = Var View String
