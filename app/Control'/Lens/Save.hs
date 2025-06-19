{-# LANGUAGE RankNTypes #-}

module Control'.Lens.Save
  ( save,
  )
where

import Control.Lens (Lens', assign, use)
import Control.Monad.State (MonadState)

save :: (MonadState s m) => Lens' s b -> m a -> m a
save l m = do
  a <- use l
  r <- m
  assign l a
  pure r
