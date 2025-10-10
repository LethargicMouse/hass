module Named (Named (..)) where

import Control.Lens (Lens')

class Named a where
  name :: Lens' a String
