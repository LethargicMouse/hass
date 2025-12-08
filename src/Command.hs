{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Command where

import Data.ByteString.Builder (Builder, intDec)
import Data.String (fromString)
import Effect.Exit (Exit, die, exitWith)
import Effect.Stdio (Stdio)
import Effectful (Eff, (:>))
import Effectful.Process (Process, readProcessWithExitCode, showCommandForUser, spawnProcess, waitForProcess)
import Render (block, quote)
import System.Exit (ExitCode (..))

run :: (Process :> es, Exit :> es) => FilePath -> [String] -> Eff es ()
run p as =
  spawnProcess p as >>= waitForProcess >>= \case
    ExitSuccess -> pure ()
    c -> exitWith c

call :: (Process :> es, Exit :> es, Stdio :> es) => FilePath -> [String] -> Eff es ()
call p as = do
  (c, o, e) <- readProcessWithExitCode p as ""
  case c of
    ExitSuccess -> pure ()
    ExitFailure c' -> die (callFail p as c' o e)

callFail :: FilePath -> [String] -> Int -> String -> String -> Builder
callFail p as c o e =
  "! error running "
    <> quote "`" (fromString $ showCommandForUser p as)
    <> ":\n--! command failed with exit code "
    <> intDec c
    <> block "stdout" (fromString o)
    <> block "stderr" (fromString e)
