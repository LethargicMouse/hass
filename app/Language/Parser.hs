module Language.Parser
  ( Parser,
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT)
import Control.Monad.Writer (Writer)
import Language.Parser.Error (Error)
import Language.Parser.State (State)

type Parser a = StateT State (ExceptT () (Writer Error)) a
