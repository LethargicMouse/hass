module Language.Trust.AST.Expr.Literal.Parser.String
  ( string,
  )
where

import Control.Applicative (many)
import Language.Parser (Parser)
import Language.Parser.Util.Str (str)
import Language.Trust.AST.Expr.Literal.Parser.String.Char (char)

string :: Parser String
string = do
  str "\""
  res <- many char
  str "\""
  pure res
