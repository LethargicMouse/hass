{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Environment (Environment, runEnvironment)
import qualified Effectful.Environment as E
import Effectful.Error.Static (Error, runError, throwError)
import Effectful.Process (Process, runProcess)
import Prelude hiding (writeFile)

main :: IO ()
main = runEffs $ do
  Args command <- getArgs
  case command of
    Clean -> clean

getArgs :: (Environment :> es, Error Builder :> es) => Eff es Args
getArgs = E.getArgs >>= parse

parse :: (Error Builder :> es) => [String] -> Eff es Args
parse [] = throwError expectedCommand
parse ("clean" : o) = case o of
  [] -> pure (Args Clean)
  a : _ -> throwError (unexpected "argument" a)
parse (c : _) = throwError (unexpected "command" c)

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
