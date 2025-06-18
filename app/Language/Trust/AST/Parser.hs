module Language.Trust.AST.Parser
  ( ast,
  )
where

import Control.Lens (use)
import Language.Parser (Parser)
import Language.Parser.State (srcName)
import Language.Parser.Util.Eof (eof)
import Language.Trust.AST (AST (..))
import Language.Trust.AST.Fun.Parser (fun)

ast :: Parser AST
ast =
  AST
    <$> use srcName
    <*> fun
    <* eof
