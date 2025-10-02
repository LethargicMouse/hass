module Link.Compiler (runLinkCompiler) where

import Die (die)
import Link.Compiler.Args (Args (..), getArgs)
import Link.Compiler.Build (build)
import Qbe.Ir (dump)
import Run.CC (runCC)
import Run.Process (runProcess)
import Run.Qbe (runQbe)
import Source (Source, fromFile)

runLinkCompiler :: IO ()
runLinkCompiler = handle =<< getArgs

handle :: Args -> IO ()
handle (Args path) = compileAndRun =<< fromFile path

compileAndRun :: Source -> IO ()
compileAndRun s = do
  compile s
  runProcess "./out" []

compile :: Source -> IO ()
compile s = do
  either die dump (build s)
  runQbe
  runCC
