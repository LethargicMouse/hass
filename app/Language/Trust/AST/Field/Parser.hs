module Language.Trust.AST.Field.Parser
  ( field,
  )
where

import Language.Parser (Parser)
import Language.Parser.Util.Name (name)
import Language.Trust.AST.Field (Field (..))

field :: Parser Field
field = Field <$> name
