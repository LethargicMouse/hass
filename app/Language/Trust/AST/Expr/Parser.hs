module Language.Trust.AST.Expr.Parser
  ( block,
  )
where

import Control.Applicative (many, (<|>))
import Data.Foldable (foldl')
import Language.Parser (Parser)
import Language.Parser.Util.Name (name)
import Language.Parser.Util.Sep (sep)
import Language.Parser.Util.Str (str)
import Language.Parser.Util.Viewed (viewed)
import qualified Language.Trust.AST.Expr.Postfix as P
import Language.Trust.Expr
  ( Block (..),
    Call (..),
    Expr (..),
    Field (..),
    Var (..),
  )

block :: Parser Block
block =
  Block
    <$ str "{"
    <*> (expr <|> pure Unit)
    <* str "}"

expr :: Parser Expr
expr = do
  e <- expr'
  ps <- many postfix
  pure (foldl' f e ps)
  where
    f e (P.Field nv n) = FieldExpr (Field e nv n)

expr' :: Parser Expr
expr' =
  Unit <$ str "()"
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
postfix = uncurry P.Field <$ str "." <*> viewed name
