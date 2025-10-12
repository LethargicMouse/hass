{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Zoom (
  zoom,
  magnify,
  retell,
  storeError,
  storeError_,
) where

import Control.Lens (Lens', assign, use, view)
import Control.Monad (void)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT (runStateT))
import Control.Monad.Writer (MonadWriter, WriterT (runWriterT), tell)

zoom :: (MonadState s m) => Lens' s z -> StateT z m a -> m a
zoom l m = do
  (a, b) <- runStateT m =<< use l
  a <$ assign l b

magnify :: (MonadReader r m) => Lens' r r' -> ReaderT r' m a -> m a
magnify l m = do
  runReaderT m =<< view l

retell :: (MonadWriter v m) => (w -> v) -> WriterT w m a -> m a
retell f m = do
  (a, w) <- runWriterT m
  a <$ tell (f w)

storeError :: (MonadWriter e' m) => (e -> e') -> ExceptT e m a -> m (Maybe a)
storeError f n =
  either
    (fmap (const Nothing) . tell . f)
    (pure . Just)
    =<< runExceptT n

storeError_ :: (MonadWriter e' m) => (e -> e') -> ExceptT e m a -> m ()
storeError_ f = void . storeError f
