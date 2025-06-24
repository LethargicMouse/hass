module Language.Trust.Run.Code
  ( runCode,
  )
where

import Language.IR.Run (run)
import Language.Trust.Compile (compile)
import Process.Die (die)

runCode :: String -> [String] -> String -> IO ()
runCode n args = either die (run args) . compile n
