-- provides a collection of useful generic functions
{-# LANGUAGE RankNTypes #-}

module Combinators where

import Control.Lens (Lens', assign, use)
import Control.Monad.Except (ExceptT, MonadError, modifyError)
import Control.Monad.RWS (MonadState)

(<!>) :: (MonadError e' m) => (e -> e') -> ExceptT e m a -> m a
(<!>) = modifyError

changing :: (MonadState s m) => Lens' s s' -> (s' -> m s') -> m ()
changing l f = use l >>= f >>= assign l
