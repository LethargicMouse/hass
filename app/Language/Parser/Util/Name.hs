module Language.Parser.Util.Name
  ( name,
    name',
  )
where

import Control.Lens (modifying, use)
import Language.Parser (Parser)
import Language.Parser.Error.Throw (failParse)
import Language.Parser.State (rest)
import Language.Parser.Util.Name.Char (isNameChar)
import Language.Parser.Util.SkipSpace (skipSpace)

name :: Parser String
name = skipSpace >> name'

name' :: Parser String
name' = do
  r <- takeWhile isNameChar . map fst <$> use rest
  if null r
    then failParse "name"
    else r <$ modifying rest (drop $ length r)
