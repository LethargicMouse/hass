-- provides `Parse` for Link `Expr` AST
module Link.AST.Expr.Parse (expr) where

import Control.Applicative (many, some, (<|>))
import Control.Lens (view)
import Data.Char (isDigit)
import Data.List (foldl')
import Enclosed (enclosed)
import Link.AST.Expr
import Link.AST.Expr.Postfix (Postfix (..))
import Source.Parse (Parse, failParse)
import Source.Parse.Common (manySep, satisfy, str, str', viewed)
import Source.Parse.Common.Name (name, name')
import Source.View (Viewed)
import Unwrap (unwrap)

expr :: Parse (Viewed Expr)
expr = foldl' addPostfix . fmap Atomic <$> viewed atom <*> many (viewed postfix)

postfix :: Parse Postfix
postfix = FieldPostfix <$ str "." <*> viewed name

atom :: Parse AtomExpr
atom =
  Unit <$ str' "()"
    <|> Let <$> letExpr
    <|> Call <$> callExpr
    <|> Var <$> varExpr
    <|> Str <$> strLiteral
    <|> Int <$> intLiteral
    <|> failParse "expr"

intLiteral :: Parse Integer
intLiteral = read <$> some (satisfy isDigit)

callExpr :: Parse CallExpr
callExpr =
  CallExpr
    <$> viewed name'
    <* str "("
    <*> manySep "," (view unwrap <$> expr)
    <* str ")"

strLiteral :: Parse String
strLiteral =
  read . enclosed "\""
    <$ str' "\""
    <*> many strChar
    <* str "\""

strChar :: Parse Char
strChar = satisfy (not . flip elem ['\\', '"'])

varExpr :: Parse (Viewed String)
varExpr = viewed name'

letExpr :: Parse LetExpr
letExpr =
  LetExpr
    <$ str' "let"
    <*> name
    <* str "="
    <*> (view unwrap <$> expr)
