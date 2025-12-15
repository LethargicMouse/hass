{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Linc.Args where

import Data.ByteString.Builder (Builder)
import Data.String (fromString)
import Effectful (Eff, runPureEff, (:>))
import Effectful.Environment (Environment)
import qualified Effectful.Environment as E
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError_)
import Effectful.State.Static.Local (State, evalState, get, modify)

newtype Args = Args {command :: Command}

data Command
  = Clean
  | Run FilePath

getArgs :: (Environment :> es, Error Builder :> es) => Eff es Args
getArgs = E.getArgs >>= liftError . parse

liftError :: (Error e :> es) => Either e a -> Eff es a
liftError = either throwError_ pure

parse :: [String] -> Either Builder Args
parse s = runPureEff . runErrorNoCallStack . evalState s $ do
  c <- parseCommand
  get >>= \case
    [] -> pure (Args c)
    a : _ -> throwError_ (unexpected "argument" a)

parseCommand :: (Error Builder :> es, State [String] :> es) => Eff es Command
parseCommand =
  expect "command" >>= \case
    "clean" -> pure Clean
    "run" -> Run <$> expect "path"
    c -> throwError_ (unexpected "command" c)

expect :: (Error Builder :> es, State [String] :> es) => Builder -> Eff es String
expect s =
  get >>= \case
    [] -> throwError_ (expected s)
    a : _ -> a <$ modify (drop @String 1)

unexpected :: Builder -> String -> Builder
unexpected k n =
  argsError
    <> "unexpected "
    <> k
    <> ": "
    <> fromString n

argsError :: Builder
argsError = "! error reading args: "

expected :: Builder -> Builder
expected a = argsError <> "expected " <> a
