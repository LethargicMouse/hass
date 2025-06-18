module Language.Trust.Checker
  ( Checker,
    runChecker,
  )
where

import Control.Monad.Except (Except, runExcept)
import Control.Monad.Reader (ReaderT, runReaderT)
import Language.Trust.Checker.Error (Error)
import Language.Trust.Program (Program)

type Checker a = ReaderT Program (Except Error) a

runChecker :: Checker a -> Program -> Either Error a
runChecker c = runExcept . runReaderT c
