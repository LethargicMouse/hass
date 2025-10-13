module Link.Compiler (
  run,
) where

import Die (die)
import Link.Compiler.Args (Args (..), getArgs)
import Link.Compiler.Build (build)
import qualified Process
import Qbe.Ir (dump)
import Run.CC (runCC)
import Run.Qbe (runQbe)
import Source (Source, fromFile)

run :: IO ()
run = runWithArgs =<< getArgs

runWithArgs :: Args -> IO ()
runWithArgs (Args path) = compileAndRun =<< fromFile path

compileAndRun :: Source -> IO ()
compileAndRun s = do
  compile s
  Process.run "./out" []

compile :: Source -> IO ()
compile s = do
  either die dump (build s)
  runQbe
  runCC
