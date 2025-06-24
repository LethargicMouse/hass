module Language.Parser.Util.Number
  ( number,
    number',
  )
where

import Control.Lens (modifying, use)
import Data.Char (isDigit)
import Language.Parser (Parser)
import Language.Parser.Error.Throw (failParse)
import Language.Parser.State (rest)
import Language.Parser.Util.SkipSpace (skipSpace)

number :: Parser Integer
number = skipSpace >> number'

number' :: Parser Integer
number' = do
  r <- takeWhile isDigit . map fst <$> use rest
  if null r
    then failParse "number"
    else read r <$ modifying rest (drop $ length r)
