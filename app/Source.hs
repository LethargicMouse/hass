module Source (
  Source (..),
  Info (..),
  Code (..),
  fromFile,
  text,
  pos,
  nextChar,
)
where

import BaseFix (head)
import Data.Vector (Vector, fromList)
import Source.Pos (Pos, posify)
import Prelude hiding (head)

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

newtype Code = Code {unCode :: [(Pos, Char)]}

text :: Code -> String
text = map snd . unCode

nextChar :: Code -> Char
nextChar = head . text

pos :: Code -> Pos
pos = fst . head . unCode
