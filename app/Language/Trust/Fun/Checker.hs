module Language.Trust.Fun.Checker
  ( fun,
  )
where

import qualified Language.IR.Fun as IR
import Language.Trust.Checker (Checker)
import Language.Trust.Expr.Checker (block)
import Language.Trust.Fun (Fun (..))

fun :: Fun -> Checker IR.Fun
fun (Fun _ body) = do
  ret <- block body
  pure (IR.Fun ret)
