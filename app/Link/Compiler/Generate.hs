{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Link.Compiler.Generate (generate) where

import Control.Lens (Lens', assign, makeLenses, modifying, use, view)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Control.Monad.State (MonadState, execState)
import Data.Map (insert, (!))
import Link.AST (Block (..), CallExpr (CallExpr), Expr (..), Fn (..), LetExpr (..), Type (..), asFn, retType)
import Link.Compiler.Generate.Stmts (ExprGen, addStmt, addStr, addTmp, newStmtsGen, stmts, vars)
import Link.Program (Program, items)
import Link.Program.Info (Info)
import Named (name)
import Qbe.Ir (IR, addFn, empty)
import qualified Qbe.Ir as IR
import Zoom (zoom)

data Gen
  = Gen
  { _stmtsGen :: ExprGen
  , _result :: IR
  }

makeLenses ''Gen

generate :: Info -> Program -> IR
generate _ p =
  view result $
    walkProgram
      `runReaderT` p
      `execState` newGen

newGen :: Gen
newGen =
  Gen
    newStmtsGen
    empty

walkProgram :: (MonadReader Program m, MonadState Gen m) => m ()
walkProgram = do
  mf <- asks (asFn . (! "main") . items)
  mf' <- walkFn (fnRequest mf)
  modifying result (addFn mf')

fnRequest :: Fn -> FnRequest
fnRequest f =
  FnRequest
    { fn = f
    }

walkFn :: (MonadState Gen m) => FnRequest -> m IR.Fn
walkFn (FnRequest f) = do
  let t = genType (retType $ header f)
  let n = name f
  ss <- replace (stmtsGen . stmts) [IR.Label "start"]
  zoom stmtsGen (walkBlock $ body f)
  ss' <- replace (stmtsGen . stmts) ss
  pure (IR.Fn t n $ reverse ss')

walkBlock :: (MonadState ExprGen m) => Block -> m ()
walkBlock (Block ss r) = do
  mapM_ walkExpr ss
  walkRet r

walkExpr :: (MonadState ExprGen m) => Expr -> m Int
walkExpr e = case e of
  Unit -> walkUnit
  Let l -> walkLet l
  Var v -> walkVar v
  Call c -> walkCall c
  Str s -> walkStr s
  Int i -> walkInt i

walkStr :: (MonadState ExprGen m) => String -> m Int
walkStr s = do
  i <- addStr s
  j <- addTmp
  addStmt . IR.Alloc $
    IR.AllocStmt
      _
      8
      16
  addStmt . IR.Store $
    IR.StoreStmt
      (IR.Basic IR.Long)
      (IR.Const $ mkConst i)
      (IR.Tmp $ tmp j)
  j' <- addTmp
  addStmt $
    IR.Add (tmp j') IR.Long (IR.Tmp $ tmp j) (IR.Int 8)
  addStmt . IR.Store $
    IR.StoreStmt
      (IR.Basic IR.Long)
      (IR.Int $ length s)
      (IR.Tmp $ tmp j')
  pure j

mkConst :: Int -> String
mkConst = ('c' :) . show

walkCall :: (MonadState ExprGen m) => CallExpr -> m Int
walkCall (CallExpr _ as) = do
  is <- mapM walkExpr as
  addStmt $
    IR.Call
      _
      _
      _
      _

walkVar :: (MonadState ExprGen m) => String -> m Int
walkVar n = (! n) <$> use vars

walkLet :: (MonadState ExprGen m) => LetExpr -> m Int
walkLet (LetExpr n e) = do
  i <- walkExpr e
  modifying vars (insert n i)
  walkUnit

walkUnit :: (MonadState ExprGen m) => m Int
walkUnit = walkInt 0

walkInt :: (MonadState ExprGen m) => Integer -> m Int
walkInt a = do
  i <- addTmp
  addStmt . IR.Copy $
    IR.CopyStmt
      (tmp i)
      IR.Word
      (IR.Int $ fromInteger a)
  pure i

walkRet :: (MonadState ExprGen m) => Expr -> m ()
walkRet e = do
  i <- walkExpr e
  addStmt (IR.Ret $ IR.Tmp $ tmp i)

tmp :: Int -> String
tmp = ('t' :) . show

replace :: (MonadState s m) => Lens' s a -> a -> m a
replace l a = use l <* assign l a

genType :: Type -> IR.Type
genType Void = IR.Word

newtype FnRequest = FnRequest
  { fn :: Fn
  }
