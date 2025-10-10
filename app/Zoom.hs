{-# LANGUAGE RankNTypes #-}

module Zoom (
  zoom,
  magnify,
) where

import Control.Lens (Lens', assign, use, view)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT (runStateT))

zoom :: (MonadState s m) => Lens' s z -> StateT z m a -> m a
zoom l m = do
  (a, b) <- runStateT m =<< use l
  a <$ assign l b

magnify :: (MonadReader r m) => Lens' r r' -> ReaderT r' m a -> m a
magnify l m = do
  runReaderT m =<< view l
