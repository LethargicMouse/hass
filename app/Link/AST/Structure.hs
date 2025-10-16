{-# LANGUAGE FlexibleContexts #-}

-- provides a function to structure Link `AST` into `Program`
module Link.AST.Structure (structure) where

import Combinators (changing)
import Control.Monad.Except (MonadError)
import Control.Monad.State (MonadState, execStateT)
import Link.AST (AST (..))
import Link.AST.Item (Item)
import Link.AST.Structure.Add (add)
import Link.AST.Structure.Error (AlreadyExists)
import Link.Program (Program, empty, items)

structure :: (MonadError AlreadyExists m) => AST -> m Program
structure (AST n is) = mapM_ addItem is `execStateT` empty n

addItem ::
  ( MonadState Program m
  , MonadError AlreadyExists m
  ) =>
  Item ->
  m ()
addItem f = changing items (add f)
