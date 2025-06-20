module Language.Trust.Expr
  ( Block (..),
    Expr (..),
    Call (..),
    Var (..),
    Field (..),
  )
where

import Language.View (View)

data Expr
  = Unit
  | CallExpr Call
  | FieldExpr Field
  | VarExpr Var

newtype Block
  = Block Expr

data Call
  = Call View String [Expr]

data Var
  = Var View String

data Field
  = Field Expr View String
