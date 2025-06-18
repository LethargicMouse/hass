module Language.Trust.AST.Expr.Parser
  ( block,
  )
where

import Control.Applicative ((<|>))
import Language.Parser (Parser)
import Language.Parser.Util.Name (name)
import Language.Parser.Util.Str (str)
import Language.Trust.Expr
  ( Block (..),
    Call (..),
    Expr (..),
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

call :: Parser Call
call = Call <$ name <* str "(" <* str ")"
