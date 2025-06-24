module Language.Trust.Expr.Literal.Checker
  ( literal,
  )
where

import qualified Language.IR.Expr as IR
import Language.Trust.Checker (Checker)
import Language.Trust.Expr.Literal (Literal (..))
import Language.Trust.Type (Type (Name))
import qualified Language.Trust.Type as T

literal :: Literal -> Checker (IR.Expr, Type)
literal Unit = pure (IR.Unit, T.Unit)
literal (Int i) = pure (IR.Int i, Name "int")
