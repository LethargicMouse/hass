module Language.Trust.AST.Fun.Header.Parser
  ( header,
  )
where

import Language.Parser (Parser)
import Language.Parser.Util.Name (name)
import Language.Parser.Util.Str (str)
import Language.Parser.Util.Viewed (viewed)
import Language.Trust.Fun.Header (Header (..))

header :: Parser Header
header = do
  str "fn"
  (nv, n) <- viewed name
  str "("
  str ")"
  pure (Header nv n)
