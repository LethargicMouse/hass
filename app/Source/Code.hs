{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- Provides data type for processing code (without metadata like source name)
module Source.Code (
  Code (..),
  consume,
) where

import Control.Lens (makeLenses, modifying)
import Control.Monad.State (MonadState)
import Source.Pos (Pos)
import Unwrap (Unwrap (..))

newtype Code = Code {_unwrap_ :: [(Pos, Char)]}

makeLenses ''Code

instance Unwrap [(Pos, Char)] Code where
  unwrap = unwrap_

consume :: (MonadState Code m) => Int -> m ()
consume = modifying unwrap . drop
