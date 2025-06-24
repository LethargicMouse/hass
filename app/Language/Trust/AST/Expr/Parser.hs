module Language.Trust.AST.Expr.Parser
  ( block,
  )
where

import Control.Applicative (many, (<|>))
import Control.Monad (when)
import Data.Foldable (foldl')
import Language.Parser (Parser)
import Language.Parser.Util.Name (name, name')
import Language.Parser.Util.Number (number)
import Language.Parser.Util.Sep (sep)
import Language.Parser.Util.Str (str)
import Language.Parser.Util.Viewed (viewed)
import Language.Trust.AST.Expr.Binary.Parser (binary)
import Language.Trust.AST.Expr.Literal.Parser (literal)
import qualified Language.Trust.AST.Expr.Postfix as P
import Language.Trust.Expr
  ( BinExpr (..),
    Block (..),
    Call (..),
    Command (..),
    Expr (..),
    Field (..),
    Get (..),
    If (..),
    Var (..),
  )
import Language.Trust.Expr.Literal (Literal (..))

block :: Parser Block
block =
  Block
    <$ str "{"
    <*> many statement
    <*> (expr <|> pure (Literal Unit))
    <* str "}"

statement :: Parser Expr
statement = do
  e <- expr
  when (needSemicolon e) (str ";")
  pure e

needSemicolon :: Expr -> Bool
needSemicolon (IfExpr (If _ t (Literal Unit))) =
  needSemicolon t
needSemicolon (IfExpr (If _ _ e)) =
  needSemicolon e
needSemicolon _ = True

expr :: Parser Expr
expr = do
  e <- expr''
  es <- many ((,) <$> binary <*> expr'')
  pure (foldl' f e es)
  where
    f a (o, b) = BinaryExpr (BinExpr a o b)

expr'' :: Parser Expr
expr'' = do
  e <- expr'
  ps <- many postfix
  pure (foldl' f e ps)
  where
    f e (P.Field nv n) = FieldExpr (Field e nv n)
    f e (P.Get i) = GetExpr (Get e i)

expr' :: Parser Expr
expr' =
  Literal <$> literal
    <|> BlockExpr <$> block
    <|> CommandExpr <$> command
    <|> IfExpr <$> if'
    <|> CallExpr <$> call
    <|> VarExpr <$> var

call :: Parser Call
call =
  uncurry Call
    <$> viewed name
    <* str "("
    <*> sep "," expr
    <* str ")"

var :: Parser Var
var = uncurry Var <$> viewed name

postfix :: Parser P.Postfix
postfix =
  uncurry P.Field <$ str "." <*> viewed name
    <|> P.Get <$ str "[" <*> number <* str "]"

if' :: Parser If
if' =
  If
    <$ str "if"
    <*> expr
    <*> expr
    <*> (str "else" *> expr <|> pure (Literal Unit))

command :: Parser Command
command =
  uncurry Command
    <$ str "@"
    <*> viewed name'
    <* str "("
    <*> sep "," (viewed expr)
    <* str ")"
