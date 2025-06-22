module Language.Trust.AST.Top.Parser
  ( top,
  )
where

import Control.Applicative ((<|>))
import Language.Parser (Parser)
import Language.Trust.AST.Alias.Parser (alias)
import Language.Trust.AST.Fun.Parser (fun)
import Language.Trust.AST.Struct.Parser (struct)
import Language.Trust.AST.Top (Top (..))

top :: Parser Top
top =
  FunTop <$> fun
    <|> StructTop <$> struct
    <|> AliasTop <$> alias
