{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Source where

import Data.ByteString.Char8 (ByteString, lines)
import Data.Vector (Vector, fromList)
import Effect.File (File, readFile)
import Effectful (Eff, (:>))
import Text (Text, render)
import Prelude hiding (lines, readFile)

data Source
  = Source Info ByteString

data Info = Info
  { sourceName :: Text
  , codeLines :: Vector Text
  }

readSource :: (File :> es) => FilePath -> Eff es Source
readSource p = source (render p) <$> readFile p

source :: Text -> ByteString -> Source
source p s = Source (Info p ls) s
 where
  ls = fromList $ (render <$> lines s) ++ ["<end of file>"]
