{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Command where

import Combinators ((>>=>))
import Control.Monad ((>=>))
import Data.ByteString.Builder (Builder, intDec)
import Data.String (fromString)
import Effect.Exit (Exit, exitWith)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Process (Process, readProcessWithExitCode, showCommandForUser, spawnProcess, waitForProcess)
import System.Exit (ExitCode (..))
import Text (block, quote)

run :: (Process :> es, Exit :> es) => FilePath -> [String] -> Eff es ()
run = spawnProcess >>=> waitForProcess >=> onFailure exitWith

onFailure :: (Applicative f) => (ExitCode -> f ()) -> ExitCode -> f ()
onFailure f = \case
  ExitSuccess -> pure ()
  c -> f c

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
