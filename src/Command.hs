{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Command where

import Data.String (fromString)
import Effectful (Eff, IOE, MonadIO (liftIO), (:>))
import Shorts (Dies, die, enclosed, runDeath)
import System.Exit (ExitCode (..), exitWith)
import System.Process (readProcessWithExitCode, showCommandForUser, spawnProcess, waitForProcess)

call :: (IOE :> es) => FilePath -> [String] -> Eff es ()
call p as =
  liftIO (readProcessWithExitCode p as "")
    >>= \(c, o, e) -> case c of
      ExitSuccess -> pure ()
      ExitFailure c' -> runDeath $ failCall p as c' o e

failCall :: (Dies es) => FilePath -> [String] -> Int -> String -> String -> Eff es a
failCall p as c o e =
  die $
    fromString $
      "! error calling "
        ++ enclosed "`" (showCommandForUser p as)
        ++ ": command failed with exit code "
        ++ show c
        ++ "\n"
        ++ block "stdout" o
        ++ "\n"
        ++ block "stderr" e

block :: String -> String -> String
block n s =
  line padding
    ++ enclosed " " n
    ++ line rest
    ++ "\n"
    ++ s
    ++ mln
    ++ line full
 where
  line m = replicate m '-'
  padding = 4
  rest = full - padding - 2
  full = 40
  mln
    | null s = ""
    | last s == '\n' = ""
    | otherwise = "\n"

run :: (IOE :> es) => FilePath -> [String] -> Eff es ()
run p as =
  liftIO (spawnProcess p as >>= waitForProcess) >>= \case
    ExitSuccess -> pure ()
    c -> liftIO (exitWith c)
