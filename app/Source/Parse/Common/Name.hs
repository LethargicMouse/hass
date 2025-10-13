-- Provides `Source` `Parse` for default programming language name (aka identifier)
module Source.Parse.Common.Name (
  name,
  name',
) where

import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import Control.Monad.State (gets)
import Data.Char (isAlpha, isDigit)
import Source.Code (consume)
import Source.Code.Getters (text)
import Source.Parse (Parse, failParse)
import Source.Parse.Common (skipSpaces)

name :: Parse String
name = name' <|> failParse "name"

name' :: Parse String
name' = do
  skipSpaces
  t <- gets (takeWhile isNameChar . text)
  if not (null t) && isName1Char (head t)
    then t <$ consume (length t)
    else throwError ()

isName1Char :: Char -> Bool
isName1Char c = isAlpha c || c == '_'

isNameChar :: Char -> Bool
isNameChar c = isName1Char c || isDigit c
