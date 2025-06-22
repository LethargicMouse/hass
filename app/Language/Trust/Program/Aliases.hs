module Language.Trust.Program.Aliases
  ( Aliases,
  )
where

import Data.Map (Map)
import Language.Trust.Alias (Alias)

type Aliases = Map String Alias
