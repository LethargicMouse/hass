module Language.Trust.AST.Alias.Parser
  ( alias,
  )
where

import Language.Parser (Parser)
import Language.Parser.Util.Name (name)
import Language.Parser.Util.Str (str)
import Language.Parser.Util.Viewed (viewed)
import Language.Trust.AST.Type.Parser (type')
import Language.Trust.Alias (Alias (..))

alias :: Parser Alias
alias =
  uncurry Alias
    <$ str "type"
    <*> viewed name
    <* str "="
    <*> type'
    <* str ";"
