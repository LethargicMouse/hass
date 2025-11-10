{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Process where

import Data.String (fromString)
import Error (die)
import System.Exit (ExitCode (..), exitWith)
import System.Process (readProcessWithExitCode, showCommandForUser, spawnProcess, waitForProcess)
import Text (Text, block, enclosed, text)

call :: String -> [String] -> IO ()
call n as = do
  (c, o, e) <- readProcessWithExitCode n as ""
  case c of
    ExitSuccess -> pure ()
    ExitFailure c' -> die (callFailed (showCommandForUser n as) c' o e)

run :: String -> [String] -> IO ()
run n as =
  spawnProcess n as >>= waitForProcess >>= \case
    ExitSuccess -> pure ()
    c -> exitWith c

callFailed :: String -> Int -> String -> String -> Text
callFailed n c o e =
  "! error running "
    <> enclosed "`" (fromString n)
    <> ":\n--! process exited with code "
    <> text c
    <> "\n"
    <> block "stdout" o
    <> block "stderr" e
