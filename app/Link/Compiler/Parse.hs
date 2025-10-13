{-# LANGUAGE FlexibleContexts #-}

-- provides `Parse` for Link `AST`
module Link.Compiler.Parse (
  ast,
) where

import Control.Applicative (many, some, (<|>))
import Control.Lens (view)
import Control.Monad (when)
import Control.Monad.Reader (asks)
import Data.Char (isDigit)
import Data.List (foldl')
import Enclosed (enclosed)
import Link.AST (AST (AST), AtomExpr (..), Block (..), CallExpr (CallExpr), Expr (..), Extern (..), Fn (Fn), Header (..), Item (..), LetExpr (..), Postfix (FieldPostfix), Type (..), addPostfix, needsSemicolon)
import Source (srcName)
import Source.Parse (Parse, failParse)
import Source.Parse.Common (eof, manySep, satisfy, str, str', viewed)
import Source.Parse.Common.Name (name, name')
import Source.View (Viewed (..))
import Unwrap (unwrap)
import Prelude hiding (head)

ast :: Parse AST
ast = asks (AST . srcName) <*> many item <* eof

item :: Parse Item
item =
  FnItem <$> fn
    <|> ExItem <$> extern

extern :: Parse Extern
extern =
  Extern
    <$ str "extern"
    <*> header

fn :: Parse Fn
fn = Fn <$> header <*> block

header :: Parse Header
header = Header <$ str "fn" <*> viewed name <* str "(" <* str ")" <*> pure Void

block :: Parse Block
block =
  Block
    <$ str "{"
    <*> many stmt
    <*> (view unwrap <$> expr <|> pure (Atomic Unit))
    <* str "}"

stmt :: Parse Expr
stmt = do
  e <- view unwrap <$> expr
  e <$ when (needsSemicolon e) (str ";")

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
