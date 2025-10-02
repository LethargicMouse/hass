{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Link.Compiler.Generate (generate) where

import Control.Lens (Lens', assign, makeLenses, modifying, use, view)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Control.Monad.State (MonadState, execState)
import Data.Map ((!))
import Link.AST (Block (..), Expr (..), Fn (..), Type (..), asFn, retType)
import Link.Compiler.Generate.Stmts (StmtsGen, addTmp, newStmtsGen, stmts)
import Link.Program (Program, items)
import Link.Program.Info (Info)
import Named (name)
import Qbe.Ir (IR, addFn, empty)
import qualified Qbe.Ir as IR
import Zoom (zoom)

data Gen
  = Gen
  { _stmtsGen :: StmtsGen,
    _result :: IR
  }

makeLenses ''Gen

generate :: Info -> Program -> IR
generate _ p =
  view result $
    walkProgram
      `runReaderT` p
      `execState` newGen

newGen :: Gen
newGen = Gen newStmtsGen empty

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

walkBlock :: (MonadState StmtsGen m) => Block -> m ()
walkBlock (Block ss r) = do
  mapM_ walkExpr ss
  walkRet r

walkExpr :: (MonadState StmtsGen m) => Expr -> m Int
walkExpr e = case e of
  Unit -> walkUnit

walkUnit :: (MonadState StmtsGen m) => m Int
walkUnit = walkInt 0

walkInt :: (MonadState StmtsGen m) => Int -> m Int
walkInt a = do
  i <- addTmp
  modifying stmts $
    (:) (IR.Copy (tmp i) IR.Word $ IR.Int a)
  pure i

walkRet :: (MonadState StmtsGen m) => Expr -> m ()
walkRet e = do
  i <- walkExpr e
  modifying stmts $
    (:) (IR.Ret $ IR.Tmp $ tmp i)

tmp :: Int -> String
tmp = ('t' :) . show

replace :: (MonadState s m) => Lens' s a -> a -> m a
replace l a = use l <* assign l a

genType :: Type -> IR.Type
genType Void = IR.Word

newtype FnRequest = FnRequest
  { fn :: Fn
  }
