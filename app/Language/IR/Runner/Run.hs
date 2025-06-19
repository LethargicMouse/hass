module Language.IR.Runner.Run
  ( runRunner,
  )
where

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Language.IR.Program (Program)
import Language.IR.Runner (Runner)
import Language.IR.Runner.State.New (new)

runRunner :: Runner a -> Program -> IO a
runRunner r = flip evalStateT new . runReaderT r
