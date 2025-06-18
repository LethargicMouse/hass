module Language.IR.Run
  ( run,
  )
where

import Language.IR.Program (Program)
import Language.IR.Program.Runner (program)
import Language.IR.Runner (runRunner)

run :: Program -> IO ()
run = runRunner program
