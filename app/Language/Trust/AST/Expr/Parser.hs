module Language.Trust.AST.Expr.Parser
  ( block,
  )
where

import Control.Applicative ((<|>))
import Language.Parser (Parser)
import Language.Parser.Util.Name (name)
import Language.Parser.Util.Str (str)
import Language.Parser.Util.Viewed (viewed)
import Language.Trust.Expr
  ( Block (..),
    Call (..),
    Expr (..),
    Var (..),
  )

block :: Parser Block
block =
  Block
    <$ str "{"
    <*> (expr <|> pure Unit)
    <* str "}"

expr :: Parser Expr
expr =
  Unit <$ str "()"
    <|> CallExpr <$> call
    <|> VarExpr <$> var

call :: Parser Call
call =
  uncurry Call
    <$> viewed name
    <* str "("
    <* expr
    <* str ")"

var :: Parser Var
var = Var <$> name
