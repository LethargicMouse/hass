module Language.Trust.Expr.Checker
  ( block,
  )
where

import Control.Lens (use, view)
import Control.Monad (forM)
import Data.Map ((!?))
import qualified Language.IR.Expr as IR
import Language.Trust.AST.Field (name)
import Language.Trust.Checker (Checker)
import Language.Trust.Checker.Error (Error (..))
import Language.Trust.Checker.Error.NotDeclared (ND (..))
import Language.Trust.Checker.State (vars)
import Language.Trust.Checker.Util.OrFail (orFail)
import Language.Trust.Expr (Block (..), Call (..), Expr (..), Var (..))
import Language.Trust.Fun.Header (params)
import Language.Trust.Fun.Header.Checker.Find (findHeader)

block :: Block -> Checker IR.Expr
block (Block ret) = do
  expr ret

expr :: Expr -> Checker IR.Expr
expr Unit = pure IR.Unit
expr (CallExpr c) = call c
expr (VarExpr v) = var v

call :: Call -> Checker IR.Expr
call (Call nv n args) = do
  h <- findHeader n nv
  as <- forM args expr
  let ps = view name <$> view params h
  pure $ IR.Block (zipWith IR.Set ps as) (IR.Call n)

var :: Var -> Checker IR.Expr
var (Var nv n) = do
  vs <- use vars
  _ <-
    (vs !? n)
      `orFail` NotDeclared
        (ND nv "variable" n)
  pure (IR.VarExpr n)
