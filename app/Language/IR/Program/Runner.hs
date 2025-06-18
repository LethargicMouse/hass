module Language.IR.Program.Runner
  ( program,
  )
where

import Control.Monad (void)
import Control.Monad.Reader (ask)
import Data.Map ((!))
import Language.IR.Fun.Runner (fun)
import Language.IR.Program (Program (..))
import Language.IR.Runner (Runner)

program :: Runner ()
program = do
  Program funs <- ask
  void $ fun (funs ! "main")
