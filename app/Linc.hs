{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Command (call)
import qualified Command as C
import Control.Monad (void)
import Data.ByteString.Builder (Builder)
import Effect.Exit (Exit, die, runExit)
import Effect.File (File, runFile)
import Effect.Stdio (Stdio, runStdio)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Environment (Environment, runEnvironment)
import Effectful.Error.Static (Error, runError)
import Effectful.Process (Process, runProcess)
import Linc.Args (Args (..), Command (..), getArgs)
import Qbe.IR (IR (..), dump)
import Source (Source, readSource)

main :: IO ()
main = runEffs $ do
  Args c <- getArgs
  case c of
    Clean -> clean
    Run path -> run path

run :: (Error Builder :> es, File :> es, Process :> es, Exit :> es) => FilePath -> Eff es ()
run p =
  readSource p
    >>= dump . compile
    >> postCompile
    >> runOut

clean :: (Process :> es) => Eff es ()
clean = void $ runError @Builder $ call "rm" ["out.qbe", "out.s", "out"]

runEffs :: Eff '[Error Builder, Stdio, Exit, Environment, File, Process, IOE] a -> IO a
runEffs =
  runEff . runProcess . runFile . runEnvironment . runExit . runStdio . die

runOut :: (Process :> es, Exit :> es) => Eff es ()
runOut = C.run "./out" []

compile :: Source -> IR
compile = const IR

postCompile :: (Process :> es, Error Builder :> es) => Eff es ()
postCompile = do
  call "qbe" ["-o", "out.s", "out.qbe"]
  call "cc" ["-o", "out", "out.s"]
