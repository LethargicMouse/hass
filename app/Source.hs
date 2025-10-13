-- Provides data type for processing any kind of source code (with metadata like source name)
module Source (
  Source (..),
  Info (..),
  fromFile,
)
where

import BaseFix (readFile)
import Data.Vector (Vector, fromList)
import Source.Code (Code (..))
import Source.Pos (posify)
import Prelude hiding (head, readFile)

data Source
  = Source Info Code

fromFile :: FilePath -> IO Source
fromFile p =
  source p <$> readFile p

source :: String -> String -> Source
source n t = Source i c
 where
  i = Info n ls
  c = Code $ zip (posify t') t'
  t' = t ++ "\0"
  ls = fromList (lines t ++ [""])

data Info
  = Info
  { srcName :: String
  , codeLines :: Vector String
  }
