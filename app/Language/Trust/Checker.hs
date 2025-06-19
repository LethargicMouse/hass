module Language.Trust.Checker
  ( Checker,
  )
where

import Control.Monad.Except (Except)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Language.Trust.Checker.Error (Error)
import Language.Trust.Checker.State (State)
import Language.Trust.Program (Program)

type Checker a = ReaderT Program (StateT State (Except Error)) a
