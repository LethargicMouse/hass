module Run.Qbe (runQbe) where

import Process (run)

runQbe :: IO ()
runQbe = run "qbe" ["-o", "out.s", "out.qbe"]
