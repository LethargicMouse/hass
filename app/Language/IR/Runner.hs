module Language.IR.Runner
  ( Runner,
    runRunner,
  )
where

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT)
import Language.IR.Program (Program)
import Language.IR.Runner.State (State)
import Language.IR.Runner.State.New (new)

type Runner a = ReaderT Program (StateT State IO) a

runRunner :: Runner a -> Program -> IO a
runRunner r = flip evalStateT new . runReaderT r
