module Language.Trust.Checker.Run
  ( runChecker,
  )
where

import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Language.Trust.Checker (Checker)
import Language.Trust.Checker.Error (Error)
import Language.Trust.Checker.State.New (new)
import Language.Trust.Program (Program)

runChecker :: Checker a -> Program -> Either Error a
runChecker c = runExcept . flip evalStateT new . runReaderT c
