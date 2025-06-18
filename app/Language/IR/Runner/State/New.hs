module Language.IR.Runner.State.New
  ( new,
  )
where

import Data.Map (empty)
import Language.IR.Runner.State (State (..))

new :: State
new = State empty
