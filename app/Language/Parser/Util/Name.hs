module Language.Parser.Util.Name
  ( name,
    name',
  )
where

import Control.Lens (modifying, use)
import Data.Char (isAlpha)
import Language.Parser (Parser)
import Language.Parser.Error.Throw (failParse)
import Language.Parser.State (rest)
import Language.Parser.Util.SkipSpace (skipSpace)

name :: Parser String
name = skipSpace >> name'

name' :: Parser String
name' = do
  r <- takeWhile isNameChar . map fst <$> use rest
  if null r
    then failParse "name"
    else r <$ modifying rest (drop $ length r)

-- TODO move it to its own module

isNameChar :: Char -> Bool
isNameChar c = c == '_' || isAlpha c
