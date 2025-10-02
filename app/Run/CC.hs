module Run.CC (runCC) where

import Run.Process (runProcess)

runCC :: IO ()
runCC = runProcess "cc" ["-o", "out", "out.s"]
