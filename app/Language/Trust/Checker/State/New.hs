module Language.Trust.Checker.State.New
  ( new,
  )
where

import Data.Map (empty)
import Language.Trust.Checker.State (State (..))

new :: State
new = State empty
