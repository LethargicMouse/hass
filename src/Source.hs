{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Source where

import Data.ByteString.Char8 (ByteString)
import Effect.File (File, readFile)
import Effectful (Eff, (:>))
import Prelude hiding (readFile)

newtype Source
  = Source ByteString

readSource :: (File :> es) => FilePath -> Eff es Source
readSource = fmap Source . readFile
