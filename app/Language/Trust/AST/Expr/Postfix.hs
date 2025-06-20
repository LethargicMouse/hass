module Language.Trust.AST.Expr.Postfix
  ( Postfix (..),
  )
where

import Language.View (View)

data Postfix
  = Field View String
