module Language.Trust.AST.Expr.Binary.Parser
  ( binary,
  )
where

import Language.Parser (Parser)
import Language.Parser.Util.Str (str)
import Language.Trust.AST.Expr.Binary (Binary (..))

binary :: Parser Binary
binary = NotEqual <$ str "!="
