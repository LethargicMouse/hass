module Language.Trust.AST.Expr.Literal.Parser.String.Char
  ( char,
  )
where

import Control.Applicative ((<|>))
import Language.Parser (Parser)
import Language.Parser.Util.Satisfy (satisfy)
import Language.Parser.Util.Str (str')

char :: Parser Char
char =
  '\n' <$ str' "\\n"
    <|> satisfy "[^\\\"\\\\]" isStringChar

isStringChar :: Char -> Bool
isStringChar c = c /= '"'
