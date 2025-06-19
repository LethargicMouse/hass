module Language.Trust.Checker
  ( Checker,
    runChecker,
  )
where

import Control.Monad.Except (Except, runExcept)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT)
import Language.Trust.Checker.Error (Error)
import Language.Trust.Checker.State (State)
import Language.Trust.Checker.State.New (new)
import Language.Trust.Program (Program)

type Checker a = ReaderT Program (StateT State (Except Error)) a

runChecker :: Checker a -> Program -> Either Error a
runChecker c = runExcept . flip evalStateT new . runReaderT c
