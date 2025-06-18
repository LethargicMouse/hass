module Language.Trust.AST.Fun.Parser
  ( fun,
  )
where

import Language.Parser (Parser)
import Language.Trust.AST.Expr.Parser (block)
import Language.Trust.AST.Fun.Header.Parser (header)
import Language.Trust.Fun (Fun (..))

fun :: Parser Fun
fun = Fun <$> header <*> block
