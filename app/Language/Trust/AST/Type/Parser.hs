module Language.Trust.AST.Type.Parser
  ( type',
  )
where

import Language.Parser (Parser)
import Language.Parser.Util.Name (name)
import Language.Trust.Type (Type (..))

type' :: Parser Type
type' = Name <$> name
