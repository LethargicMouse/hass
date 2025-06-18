module Language.Trust.Expr
  ( Block (..),
    Expr (..),
    Call (..),
  )
where

data Expr
  = Unit
  | CallExpr Call

newtype Block
  = Block Expr

data Call
  = Call
