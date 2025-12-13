module Source where

import Data.ByteString.Char8 (ByteString)
import Effectful (Eff)
import Prelude hiding (readFile)

newtype Source
  = Source ByteString

readSource :: FilePath -> Eff es Source
readSource = fmap Source . readFile
