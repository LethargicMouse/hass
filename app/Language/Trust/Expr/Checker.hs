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
import qualified Language.Trust.Checker.State.Var as C
import Language.Trust.Checker.Util.OrFail (orFail)
import Language.Trust.Expr (Block (..), Call (..), Expr (..), Field (..), Var (..))
import Language.Trust.Fun.Header (params, retType)
import Language.Trust.Fun.Header.Checker.Find (findHeader)
import qualified Language.Trust.Struct.Field as F
import qualified Language.Trust.Struct.Field.Find as FF
import Language.Trust.Type (Type)
import qualified Language.Trust.Type as T

block :: Block -> Checker IR.Expr
block (Block ret) = do
  fst <$> expr ret

expr :: Expr -> Checker (IR.Expr, Type)
expr Unit = pure (IR.Unit, T.Unit)
expr (CallExpr c) = call c
expr (VarExpr v) = var v
expr (FieldExpr f) = field f

call :: Call -> Checker (IR.Expr, Type)
call (Call nv n args) = do
  h <- findHeader n nv
  as <- map fst <$> forM args expr
  let ps = view name <$> view params h
  pure
    ( IR.Block (zipWith IR.Set ps as) (IR.Call n),
      view retType h
    )

var :: Var -> Checker (IR.Expr, Type)
var (Var nv n) = do
  vs <- use vars
  C.Var t <-
    (vs !? n)
      `orFail` NotDeclared
        (ND nv "variable" n)
  pure (IR.VarExpr n, t)

field :: Field -> Checker (IR.Expr, Type)
field (Field e nv n) = do
  (e', t) <- expr e
  F.Field i t' <- FF.findField t nv n
  pure (IR.Get e' i, t')
