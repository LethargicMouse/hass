module Language.IR.Runner
  ( Runner,
  )
where

import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Language.IR.Program (Program)
import Language.IR.Runner.State (State)

-- monad for running IR
-- assuming program is correct
type Runner a = ReaderT Program (StateT State IO) a
