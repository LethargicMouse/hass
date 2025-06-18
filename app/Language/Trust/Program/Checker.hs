module Language.Trust.Program.Checker
  ( program,
  )
where

import Control.Lens (view)
import qualified Language.IR.Program as IR
import Language.Trust.Checker (Checker)
import Language.Trust.Fun.Checker (fun)
import Language.Trust.Program (funs)
import Language.Trust.Program.Checker.Main (main)

program :: Checker IR.Program
program = do
  fs <- mapM fun =<< view funs
  main
  pure (IR.Program fs)
