module Language.Trust.Program.Check
  ( check,
  )
where

import qualified Language.IR.Program as IR
import Language.Trust.Checker (runChecker)
import Language.Trust.Checker.Error (Error)
import Language.Trust.Program (Program)
import Language.Trust.Program.Checker (program)

check :: Program -> Either Error IR.Program
check = runChecker program
