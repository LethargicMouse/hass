module Run.CC (runCC) where

import Process (run)

runCC :: IO ()
runCC = run "cc" ["-o", "out", "out.s"]
