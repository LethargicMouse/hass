-- provides a collection of useful generic functions
module Combinators where

import Control.Monad.Except (ExceptT, MonadError, modifyError)

(<!>) :: (MonadError e' m) => (e -> e') -> ExceptT e m a -> m a
(<!>) = modifyError
