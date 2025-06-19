module Language.Trust.AST.Fun.Header.Parser
  ( header,
  )
where

import Control.Applicative (many)
import Language.Parser (Parser)
import Language.Parser.Util.Name (name)
import Language.Parser.Util.Str (str)
import Language.Parser.Util.Viewed (viewed)
import Language.Trust.AST.Field.Parser (field)
import Language.Trust.Fun.Header (Header (..))

header :: Parser Header
header = do
  str "fn"
  (nv, n) <- viewed name
  str "("
  ps <- many field
  str ")"
  pure (Header nv n ps)
