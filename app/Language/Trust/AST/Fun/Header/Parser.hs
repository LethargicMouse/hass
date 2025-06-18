module Language.Trust.AST.Fun.Header.Parser
  ( header,
  )
where

import Language.Parser (Parser)
import Language.Parser.Util.Name (name)
import Language.Parser.Util.Str (str)
import Language.Trust.Fun.Header (Header (..))

header :: Parser Header
header = do
  str "fn"
  n <- name
  str "("
  str ")"
  pure (Header n)
