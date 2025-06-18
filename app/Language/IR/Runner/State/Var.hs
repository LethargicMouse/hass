module Language.IR.Runner.State.Var
  ( Var (..),
  )
where

import Language.IR.Expr (Expr)

newtype Var
  = Var Expr
