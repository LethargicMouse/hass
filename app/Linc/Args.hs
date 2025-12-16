{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Linc.Args where

import Combinators ((-$))
import Effectful (Eff, (:>))
import Effectful.Environment (Environment)
import qualified Effectful.Environment as E
import Effectful.Error.Static (Error, throwError_)
import Effectful.State.Static.Local (State, evalState, get, modify)
import Text (Text, render)

newtype Args = Args {command :: Command}

data Command
  = Clean
  | Run FilePath

getArgs :: (Environment :> es, Error Text :> es) => Eff es Args
getArgs = parse =<< E.getArgs

parse :: (Error Text :> es) => [String] -> Eff es Args
parse =
  evalState -$ do
    c <- parseCommand
    \case
      [] -> pure (Args c)
      a : _ -> throwError_ (unexpected "argument" a)
      =<< get

parseCommand ::
  ( Error Text :> es
  , State [String] :> es
  ) =>
  Eff es Command
parseCommand =
  \case
    "clean" -> pure Clean
    "run" -> Run <$> expect "path"
    c -> throwError_ (unexpected "command" c)
    =<< expect "command"

expect ::
  (Error Text :> es, State [String] :> es) =>
  Text ->
  Eff es String
expect s =
  \case
    [] -> throwError_ (expected s)
    a : _ -> a <$ modify (drop @String 1)
    =<< get

unexpected :: Text -> String -> Text
unexpected k n =
  argsError $
    "unexpected "
      <> k
      <> ": "
      <> render n

argsError :: Text -> Text
argsError = ("! error reading args: " <>)

expected :: Text -> Text
expected = argsError . ("expected " <>)
