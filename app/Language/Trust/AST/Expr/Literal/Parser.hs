module Language.Trust.AST.Expr.Literal.Parser
  ( literal,
  )
where

import Control.Applicative ((<|>))
import Language.Parser (Parser)
import Language.Parser.Util.Number (number)
import Language.Parser.Util.Str (str)
import Language.Trust.Expr.Literal (Literal (..))

literal :: Parser Literal
literal =
  Unit <$ str "()"
    <|> Int <$> number
