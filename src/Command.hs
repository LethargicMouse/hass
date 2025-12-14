{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Command where

import Data.ByteString.Builder (Builder, intDec)
import Data.String (fromString)
import Effect.Exit (Exit, exitWith)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Process (Process, readProcessWithExitCode, showCommandForUser, spawnProcess, waitForProcess)
import System.Exit (ExitCode (..))
import Text (block, quote)

run :: (Process :> es, Exit :> es) => FilePath -> [String] -> Eff es ()
run p as =
  spawnProcess p as >>= waitForProcess >>= \case
    ExitSuccess -> pure ()
    c -> exitWith c

call :: (Process :> es, Error Builder :> es) => FilePath -> [String] -> Eff es ()
call p as = do
  (c, o, e) <- readProcessWithExitCode p as ""
  case c of
    ExitSuccess -> pure ()
    ExitFailure c' -> throwError (callFail p as c' o e)

callFail :: FilePath -> [String] -> Int -> String -> String -> Builder
callFail p as c o e =
  "! error running "
    <> quote "`" (fromString $ showCommandForUser p as)
    <> ":\n--! command failed with exit code "
    <> intDec c
    <> block "stdout" (fromString o)
    <> block "stderr" (fromString e)
