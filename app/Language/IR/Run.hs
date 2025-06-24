module Language.IR.Run
  ( run,
  )
where

import Language.IR.Program (Program)
import Language.IR.Program.Runner (program)
import Language.IR.Runner.Run (runRunner)

run :: [String] -> Program -> IO ()
run = runRunner . program
