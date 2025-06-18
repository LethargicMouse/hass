module Language.IR.Fun.Runner
  ( fun,
  )
where

import Language.IR.Expr (Expr)
import Language.IR.Expr.Runner (expr)
import Language.IR.Fun (Fun (..))
import Language.IR.Runner (Runner)

fun :: Fun -> Runner Expr
fun (Fun ret) = expr ret
