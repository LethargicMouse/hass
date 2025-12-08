{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Command (call, run)
import Data.ByteString.Builder (Builder)
import Effect.Exit (Exit, runExit)
import Effect.File (File, runFile, writeFile)
import Effect.Stdio (Stdio, runStdio)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Process (Process, runProcess)
import Prelude hiding (writeFile)

main :: IO ()
main =
  runEffs $
    readCode
      >>= dump . compile
      >> postCompile
      >> runOut

runEffs :: Eff '[Stdio, File, Exit, Process, IOE] a -> IO a
runEffs = runEff . runProcess . runExit . runFile . runStdio

runOut :: (Process :> es, Exit :> es) => Eff es ()
runOut = run "./out" []

readCode :: Eff es Source
readCode = pure Source

dump :: (File :> es) => IR -> Eff es ()
dump = writeFile "out.qbe" . render

render :: IR -> Builder
render _ = ""

data Source = Source

data IR
  = IR

compile :: Source -> IR
compile = const IR

postCompile :: (Process :> es, Exit :> es, Stdio :> es) => Eff es ()
postCompile = do
  call "qbe" ["-o", "out.s", "out.qbe"]
  call "cc" ["-o", "out", "out.s"]
