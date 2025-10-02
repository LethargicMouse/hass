module Run.Qbe (runQbe) where
import Run.Process (runProcess)

runQbe :: IO ()
runQbe = runProcess "qbe" ["-o", "out.s", "out.qbe"]
