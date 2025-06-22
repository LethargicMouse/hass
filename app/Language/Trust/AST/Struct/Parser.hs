module Language.Trust.AST.Struct.Parser
  ( struct,
  )
where

import Language.Parser (Parser)
import Language.Parser.Util.Name (name)
import Language.Parser.Util.Sep (sep)
import Language.Parser.Util.Str (str)
import Language.Parser.Util.Viewed (viewed)
import Language.Trust.AST.Field.Parser (field)
import Language.Trust.AST.Struct (Struct (..))

struct :: Parser Struct
struct =
  uncurry Struct
    <$ str "struct"
    <*> viewed name
    <* str "{"
    <*> sep "," field
    <* str "}"
