-- provides functions to access parts of `Code`
module Source.Code.Getters where

import Source.Code (Code (..))
import Source.Pos (Pos)
import Unwrap (un)

text :: Code -> String
text = map snd . un

nextChar :: Code -> Char
nextChar = head . text

nextPos :: Code -> Pos
nextPos = fst . head . un
