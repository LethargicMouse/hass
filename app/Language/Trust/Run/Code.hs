module Language.Trust.Run.Code
  ( runCode,
  )
where

import Language.IR.Run (run)
import Language.Trust.Compile (compile)
import Process.Die (die)

runCode :: String -> String -> IO ()
runCode n = either die run . compile n
