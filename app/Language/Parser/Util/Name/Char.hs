module Language.Parser.Util.Name.Char
  ( isNameChar,
  )
where

import Data.Char (isAlpha)

isNameChar :: Char -> Bool
isNameChar c = c == '_' || isAlpha c
