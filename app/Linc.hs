{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Writer (execWriter, tell)
import Data.String (fromString)
import Error (die)
import Process (call, run)
import Source (Source, readSource)
import System.Environment (getArgs)
import Text (Text, enclosed)

main :: IO ()
main =
  readCode
    >>= compile
    >> runOut

readCode :: IO Code
readCode = getPath >>= readSource

getPath :: IO FilePath
getPath =
  getArgs >>= \case
    [] -> die expectedPath
    [p] -> return p
    _ : a : _ -> die (unexpected a)

expectedPath :: Text
expectedPath = argsError "expected path to file"

unexpected :: String -> Text
unexpected a =
  argsError $ "unexpected argument: " <> enclosed "`" (fromString a)

argsError :: Text -> Text
argsError e = "! error reading args: " <> e

type Code = Source

compile :: Code -> IO ()
compile _ = do
  dump genIR
  runQbe
  runCC

dump :: IR -> IO ()
dump = writeFile "out.qbe" . show

type IR = Text

genIR :: IR
genIR = execWriter $ do
  tell
    "export function w $main ( ) {\n\
    \@start\n\
    \  ret 0\n\
    \}"

runQbe :: IO ()
runQbe = call "qbe" ["-o", "out.s", "out.qbe"]

runCC :: IO ()
runCC = call "cc" ["-o", "out", "out.s"]

runOut :: IO ()
runOut = run "./out" []
