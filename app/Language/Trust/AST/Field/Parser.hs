module Language.Trust.AST.Field.Parser
  ( field,
  )
where

import Language.Parser (Parser)
import Language.Parser.Util.Name (name)
import Language.Trust.AST.Field (Field (..))
import Language.Trust.Type (Type (..))

field :: Parser Field
field = (Field <*> Name) <$> name
