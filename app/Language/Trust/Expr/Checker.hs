module Language.Trust.Expr.Checker
  ( block,
  )
where

import qualified Language.IR.Expr as IR
import Language.Trust.Checker (Checker)
import Language.Trust.Expr (Block (..), Call (..), Expr (..), Var (..))
import Language.Trust.Fun.Header.Checker.Find (findHeader)

block :: Block -> Checker IR.Expr
block (Block ret) = do
  expr ret

expr :: Expr -> Checker IR.Expr
expr Unit = pure IR.Unit
expr (CallExpr c) = call c
expr (VarExpr v) = var v

call :: Call -> Checker IR.Expr
call (Call nv n) = do
  _ <- findHeader n nv
  pure (IR.Call n)

var :: Var -> Checker IR.Expr
var (Var s) = pure (IR.VarExpr s)
