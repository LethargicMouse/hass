{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Link.Compiler.Build (build) where

import Control.Lens (assign, use, (^.))
import Control.Monad.Except (MonadError, modifyError, throwError)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (MonadState, execStateT)
import Data.Map (Map, insert)
import qualified Data.Map as M
import Link.AST (AST (AST), Item)
import Link.Compiler.Analyse (analyse)
import qualified Link.Compiler.Analyse as Analyse
import Link.Compiler.Generate (generate)
import Link.Compiler.Parse (ast, parse)
import qualified Link.Compiler.Parse.Error as Parse
import Link.Program (Program, empty, items)
import Named (Named (..))
import OfKind (OfKind (..))
import Qbe.Ir (IR (..))
import Source (Source)
import Source.View (HasView (..), View)
import String.Enclosed (enclosed)

build :: (MonadError Error m) => Source -> m IR
build s = do
  a <- modifyError P (parse ast s)
  p <- modifyError AE (fromAST a)
  i <- modifyError A (analyse `runReaderT` p)
  pure (generate i p)

fromAST :: (MonadError AlreadyExists m) => AST -> m Program
fromAST (AST n is) = mapM_ addItem is `execStateT` empty n

addItem :: (MonadState Program m, MonadError AlreadyExists m) => Item -> m ()
addItem f = do
  is <- use items
  is' <- add f is
  assign items is'

add ::
  ( Named a
  , MonadError AlreadyExists m
  , HasView a
  , OfKind a
  ) =>
  a ->
  Map String a ->
  m (Map String a)
add a m =
  let n = a ^. name
   in case M.lookup n m of
        Nothing -> pure (insert n a m)
        Just a' ->
          throwError $
            AlreadyExists
              (a ^. view)
              (kind a)
              n
              (a' ^. view)

data AlreadyExists
  = AlreadyExists View String String View

instance Show AlreadyExists where
  show (AlreadyExists v n k v') =
    "! error "
      ++ show v
      ++ "\n--! "
      ++ k
      ++ enclosed " `" n
      ++ "is already declared "
      ++ show v'

data Error
  = P Parse.Error
  | A Analyse.Error
  | AE AlreadyExists

instance Show Error where
  show (P e) = show e
  show (A e) = show e
  show (AE e) = show e
