module Language.Trust.Expr.Checker
  ( block,
  )
where

import Control.Lens (use, view)
import Control.Monad (forM)
import Data.Map (Map, fromList, (!?))
import qualified Language.IR.Expr as IR
import Language.Trust.AST.Expr.Binary (Binary (..))
import Language.Trust.AST.Field (name)
import Language.Trust.Checker (Checker)
import Language.Trust.Checker.Error (Error (..))
import Language.Trust.Checker.Error.NotDeclared (ND (..))
import Language.Trust.Checker.Error.UnknownCommand (UC (..))
import Language.Trust.Checker.State (vars)
import qualified Language.Trust.Checker.State.Var as C
import Language.Trust.Checker.Util.OrFail (orFail)
import Language.Trust.Expr (BinExpr (..), Block (..), Call (..), Command (..), Expr (..), Field (..), Get (..), If (..), Var (..))
import Language.Trust.Expr.Literal.Checker (literal)
import Language.Trust.Fun.Header (params, retType)
import Language.Trust.Fun.Header.Checker.Find (findHeader)
import qualified Language.Trust.Struct.Field as F
import qualified Language.Trust.Struct.Field.Find as FF
import Language.Trust.Type (Type (..))
import Language.Trust.Type.Elem (elemType)
import Language.View (View)
import Prelude hiding (print)

block :: Block -> Checker (IR.Expr, Type)
block (Block es ret) = do
  es' <- mapM (fmap fst . expr) es
  (ret', t) <- expr ret
  pure (IR.Block es' ret', t)

expr :: Expr -> Checker (IR.Expr, Type)
expr (Literal l) = literal l
expr (CallExpr c) = call c
expr (VarExpr v) = var v
expr (FieldExpr f) = field f
expr (BinaryExpr b) = binExpr b
expr (IfExpr i) = if' i
expr (BlockExpr b) = block b
expr (CommandExpr c) = command c
expr (GetExpr g) = get g

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
  F.Field _ i t' <- FF.findField t nv n
  pure (IR.Get e' i, t')

binExpr :: BinExpr -> Checker (IR.Expr, Type)
binExpr (BinExpr a o b) = do
  (a', _) <- expr a
  (b', _) <- expr b
  pure $ case o of
    NotEqual -> (IR.Not $ IR.Equal a' b', Name "bool")

if' :: If -> Checker (IR.Expr, Type)
if' (If c t e) = do
  (c', _) <- expr c
  (t', tt) <- expr t
  (e', _) <- expr e
  pure (IR.If c' t' e', tt)

command :: Command -> Checker (IR.Expr, Type)
command (Command nv n args) = do
  c <- (commands !? n) `orFail` UnknownCommand (UC nv n)
  c args

commands :: Map String ([(View, Expr)] -> Checker (IR.Expr, Type))
commands =
  fromList
    [ ("print", print)
    ]

print :: [(View, Expr)] -> Checker (IR.Expr, Type)
print es = do
  es' <- forM es (fmap fst . expr . snd)
  pure (IR.Block (IR.Print <$> es') IR.Unit, Unit)

get :: Get -> Checker (IR.Expr, Type)
get (Get e i) = do
  (e', t) <- expr e
  t' <- elemType t
  pure (IR.Get e' $ fromInteger i, t')
