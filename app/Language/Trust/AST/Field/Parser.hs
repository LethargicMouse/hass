module Language.Trust.AST.Field.Parser
  ( field,
  )
where

import Control.Applicative ((<|>))
import Language.Parser (Parser)
import Language.Parser.Util.Name (name)
import Language.Parser.Util.Str (str)
import Language.Parser.Util.Viewed (viewed)
import Language.Trust.AST.Field (Field (..))
import Language.Trust.AST.Type.Parser (type')
import Language.Trust.Type (Type (..))

field :: Parser Field
field =
  uncurry Field
    <$> viewed name
    <* str ":"
    <*> type'
    <|> (uncurry Field <*> Name . snd)
      <$> viewed name
