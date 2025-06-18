module Language.Trust.AST.Field.Parser
  ( field,
  )
where

import Control.Monad (void)
import Language.Parser (Parser)
import Language.Parser.Util.Name (name)

field :: Parser ()
field = void name
