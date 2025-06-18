module Language.IR.Runner
  ( Runner,
    runRunner,
  )
where

import Control.Monad.Reader (ReaderT, runReaderT)
import Language.IR.Program (Program)

type Runner a = ReaderT Program IO a

runRunner :: Runner a -> Program -> IO a
runRunner = runReaderT
