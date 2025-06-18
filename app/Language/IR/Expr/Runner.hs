module Language.IR.Expr.Runner
  ( expr,
  )
where

import Language.IR.Expr (Expr (..))
import Language.IR.Runner (Runner)

expr :: Expr -> Runner Expr
expr Unit = pure Unit
