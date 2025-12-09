{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Command (call, run)
import Control.Monad (void)
import Data.ByteString.Builder (Builder)
import Data.String (fromString)
import Effect.Exit (Exit, die, runExit)
import Effect.File (File, runFile, writeFile)
import Effect.Stdio (Stdio, runStdio)
import Effectful (Eff, IOE, runEff, runPureEff, (:>))
import Effectful.Environment (Environment, runEnvironment)
import qualified Effectful.Environment as E
import Effectful.Error.Static (Error, runError, runErrorNoCallStack, throwError_)
import Effectful.Process (Process, runProcess)
import Effectful.State.Static.Local (State, evalState, get, modify)
import Prelude hiding (error, writeFile)

main :: IO ()
main = runEffs $ do
  Args c <- getArgs
  case c of
    Clean -> clean

getArgs :: (Environment :> es, Error Builder :> es) => Eff es Args
getArgs = E.getArgs >>= liftError . parse

liftError :: (Error e :> es) => Either e a -> Eff es a
liftError = either throwError_ pure

parse :: [String] -> Either Builder Args
parse s = runPureEff . runErrorNoCallStack . evalState s $ do
  c <- command
  get >>= \case
    [] -> pure (Args c)
    a : _ -> throwError_ (unexpected "argument" a)

command :: (Error Builder :> es, State [String] :> es) => Eff es Command
command =
  get >>= \case
    [] -> throwError_ expectedCommand
    c : _ ->
      modify (drop @String 1) >> case c of
        "clean" -> pure Clean
        _ -> throwError_ (unexpected "command" c)

unexpected :: Builder -> String -> Builder
unexpected k n =
  argsError
    <> "unexpected "
    <> k
    <> ": "
    <> fromString n

argsError :: Builder
argsError = "! error reading args: "

expectedCommand :: Builder
expectedCommand = argsError <> "expected command"

clean :: (Process :> es) => Eff es ()
clean = void $ runError @Builder $ call "rm" ["out.qbe", "out.s", "out"]

newtype Args = Args Command

data Command = Clean

runEffs :: Eff '[Error Builder, Stdio, Exit, Environment, File, Process, IOE] a -> IO a
runEffs =
  runEff
    . runProcess
    . runFile
    . runEnvironment
    . runExit
    . runStdio
    . die

runOut :: (Process :> es, Exit :> es) => Eff es ()
runOut = run "./out" []

readCode :: Eff es Source
readCode = pure Source

dump :: (File :> es) => IR -> Eff es ()
dump = writeFile "out.qbe" . render

render :: IR -> Builder
render _ = "export function w $main() {\n@start\nret 0\n}"

data Source = Source

data IR
  = IR

compile :: Source -> IR
compile = const IR

postCompile :: (Process :> es, Error Builder :> es) => Eff es ()
postCompile = do
  call "qbe" ["-o", "out.s", "out.qbe"]
  call "cc" ["-o", "out", "out.s"]
