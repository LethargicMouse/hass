{-# LANGUAGE RankNTypes #-}

module Zoom (zoom) where

import Control.Lens (Lens', assign, use)
import Control.Monad.State (MonadState, StateT (runStateT))

zoom :: (MonadState s m) => Lens' s z -> StateT z m a -> m a
zoom l m = do
  (a, b) <- runStateT m =<< use l
  a <$ assign l b
