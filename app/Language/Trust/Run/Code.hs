module Language.Trust.Run.Code
  ( runCode,
  )
where

import Debug.Trace (trace)
import Language.IR.Run (run)
import Language.Trust.Compile (compile)
import Process.Die (die)

runCode :: String -> String -> IO ()
runCode n = either die (run . (trace <$> show <*> id)) . compile n
