{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Link.Compiler.Generate (generate) where

import Control.Lens (Lens', assign, makeLenses, modifying, to, use, (^.))
import qualified Control.Lens as L
import Control.Monad.Reader (MonadReader, runReaderT)
import Control.Monad.State (MonadState, execState)
import Data.Hashable (hash)
import Data.Map (insert, (!))
import Debug.Trace (trace)
import Link.AST (AtomExpr (..), Block (..), CallExpr (CallExpr), Expr (..), FieldExpr (..), Fn, LetExpr (..), Type (..), asFn, asName, body, header, retType)
import Link.Compiler.Generate.Stmts (ExprGen, addStmt, addStr, addTmp, newStmtsGen, stmts, vars)
import Link.Program (Program, items)
import Link.Program.Info (Info, offsets, types)
import Named (name)
import Qbe.Ir (IR, addFn, empty)
import qualified Qbe.Ir as IR
import Source.View (Viewed, un, view)
import Unwrap (unwrap)
import Zoom (magnify, zoom)

data Gen
  = Gen
  { _stmtsGen :: ExprGen
  , _result :: IR
  }

data Input
  = Input
  { _info :: Info
  , _program :: Program
  }

makeLenses ''Gen
makeLenses ''Input

generate :: Info -> Program -> IR
generate i p =
  walkProgram
    `runReaderT` Input (trace (show i) i) p
    `execState` newGen
    ^. result

newGen :: Gen
newGen =
  Gen
    newStmtsGen
    empty

walkProgram :: (MonadReader Input m, MonadState Gen m) => m ()
walkProgram = do
  mf <- L.view (program . items . to (! "main") . to asFn)
  mf' <- magnify info $ walkFn (fnRequest mf)
  modifying result (addFn mf')

fnRequest :: Fn -> FnRequest
fnRequest f =
  FnRequest
    { fn = f
    }

walkFn :: (MonadState Gen m, MonadReader Info m) => FnRequest -> m IR.Fn
walkFn (FnRequest f) = do
  let t = IR.basicAbi $ genType (f ^. header . retType)
  let n = f ^. name
  ss <- replace (stmtsGen . stmts) [IR.Label "start"]
  zoom stmtsGen (walkBlock $ f ^. body)
  ss' <- replace (stmtsGen . stmts) ss
  pure (IR.Fn t n $ reverse ss')

walkBlock :: (MonadState ExprGen m, MonadReader Info m) => Block -> m ()
walkBlock (Block ss r) = do
  mapM_ walkExpr ss
  walkRet r

walkExpr :: (MonadState ExprGen m, MonadReader Info m) => Expr -> m Int
walkExpr (Atomic e) = walkAtom e
walkExpr (Field e) = walkField e

walkField :: (MonadState ExprGen m, MonadReader Info m) => FieldExpr -> m Int
walkField (FieldExpr e f) = do
  i <- walkExpr (e ^. unwrap)
  o <- offset (e ^. view . to hash) (un f)
  i' <- addTmp
  addStmt . IR.Bin $
    IR.BinStmt
      (tmp i')
      IR.Word
      (IR.Tmp $ tmp i)
      IR.Add
      (IR.Int o)
  pure i'

offset :: (MonadReader Info m) => Int -> String -> m Int
offset i n = do
  s <- L.view (types . to (! i) . to asName)
  L.view (offsets . to (! s) . to (! n))

walkAtom :: (MonadState ExprGen m, MonadReader Info m) => AtomExpr -> m Int
walkAtom Unit = walkUnit
walkAtom (Let e) = walkLet e
walkAtom (Call e) = walkCall e
walkAtom (Var e) = walkVar e
walkAtom (Str e) = walkStr e
walkAtom (Int e) = walkInt e

walkStr :: (MonadState ExprGen m) => String -> m Int
walkStr s = do
  i <- addStr s
  j <- addTmp
  addStmt . IR.Alloc $
    IR.AllocStmt
      (tmp j)
      8
      16
  addStmt . IR.Store $
    IR.StoreStmt
      (IR.Basic IR.Long)
      (IR.Const $ mkConst i)
      (IR.Tmp $ tmp j)
  j' <- addTmp
  addStmt . IR.Bin $
    IR.BinStmt (tmp j') IR.Long (IR.Tmp $ tmp j) IR.Add (IR.Int 8)
  addStmt . IR.Store $
    IR.StoreStmt
      (IR.Basic IR.Long)
      (IR.Int $ length s)
      (IR.Tmp $ tmp j')
  pure j

mkConst :: Int -> String
mkConst = ('c' :) . show

walkCall :: (MonadState ExprGen m, MonadReader Info m) => CallExpr -> m Int
walkCall (CallExpr n as) = do
  is <- mapM walkExpr as
  i <- addTmp
  addStmt . IR.Call $
    IR.CallStmt
      (tmp i)
      IR.abiWord
      (IR.Const $ un n)
      ((IR.abiWord,) . IR.Tmp . tmp <$> is)
  pure i

walkVar :: (MonadState ExprGen m) => Viewed String -> m Int
walkVar n = (! un n) <$> use vars

walkLet :: (MonadState ExprGen m, MonadReader Info m) => LetExpr -> m Int
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

walkRet :: (MonadState ExprGen m, MonadReader Info m) => Expr -> m ()
walkRet e = do
  i <- walkExpr e
  addStmt (IR.Ret $ IR.Tmp $ tmp i)

tmp :: Int -> String
tmp = ('t' :) . show

replace :: (MonadState s m) => Lens' s a -> a -> m a
replace l a = use l <* assign l a

genType :: Type -> IR.AbiType
genType Void = IR.abiWord
genType (Name n) = IR.Name n
genType I32 = IR.abiWord

newtype FnRequest = FnRequest
  { fn :: Fn
  }
