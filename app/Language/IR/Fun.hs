module Language.IR.Fun
  ( Fun (..),
  )
where

import Language.IR.Expr (Expr)

newtype Fun
  = Fun Expr
