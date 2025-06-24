module Language.Trust.AST.Type.Parser
  ( type',
  )
where

import Control.Applicative ((<|>))
import Language.Parser (Parser)
import Language.Parser.Util.Name (name)
import Language.Parser.Util.Str (str)
import Language.Trust.Type (Type (..))

type' :: Parser Type
type' =
  Ref <$ str "&" <*> type'
    <|> Name <$> name
