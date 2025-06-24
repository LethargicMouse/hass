module Language.Trust.Fun.Checker
  ( fun,
  )
where

import Control'.Lens.Save (save)
import qualified Language.IR.Fun as IR
import Language.Trust.Checker (Checker)
import Language.Trust.Checker.State (vars)
import Language.Trust.Expr.Checker (block)
import Language.Trust.Fun (Fun (..))
import Language.Trust.Fun.Header.Checker (header)

fun :: Fun -> Checker IR.Fun
fun (Fun h b) = save vars $ do
  header h
  (ret, _) <- block b
  pure (IR.Fun ret)
