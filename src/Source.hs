{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Source where

import Data.ByteString.Char8 (ByteString)
import Data.String (IsString (fromString))
import Effect.File (File, readFile)
import Effectful (Eff, (:>))
import Text (Text)
import Prelude hiding (readFile)

data Source
  = Source Text ByteString

readSource :: (File :> es) => FilePath -> Eff es Source
readSource p = Source (fromString p) <$> readFile p
