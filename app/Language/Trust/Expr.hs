module Language.Trust.Expr
  ( Block (..),
    Expr (..),
    Call (..),
  )
where

import Language.View (View)

data Expr
  = Unit
  | CallExpr Call

newtype Block
  = Block Expr

data Call
  = Call View String
