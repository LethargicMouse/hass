module Language.Trust.AST.Expr.Literal.Parser.String.Char
  ( char,
  )
where

import Language.Parser (Parser)
import Language.Parser.Util.Satisfy (satisfy)

char :: Parser Char
char = satisfy "[^\"]" isStringChar

isStringChar :: Char -> Bool
isStringChar c = c /= '"'
