{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Code where

import Data.ByteString (ByteString)
import Effectful (Eff, (:>))
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO.ByteString (readFile)
import Prelude hiding (readFile)

data Code
  = Code String ByteString

getCode :: (FileSystem :> es) => FilePath -> Eff es Code
getCode p = Code p <$> readFile p
