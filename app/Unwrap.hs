{-# LANGUAGE FunctionalDependencies #-}

module Unwrap (Unwrap (..), un) where

import Control.Lens (Lens', view)

class Unwrap a t | t -> a where
  unwrap :: Lens' t a

un :: (Unwrap a t) => t -> a
un = view unwrap
