{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Command (call, run)
import Control.Monad ((>=>))
import Data.String (IsString (fromString))
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Environment (Environment, getArgs, runEnvironment)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.FileSystem.IO.ByteString (readFile, writeFile)
import Link.Analyse (analyse)
import Link.Lex (Code (..), lex)
import Link.Parse (ast, parse)
import Qbe.IR (IR)
import Shorts (Dies, die, die', enclosed, runDeath)
import Prelude hiding (lex, readFile, writeFile)

main :: IO ()
main =
  runEff $
    runDeath $
      runFileSystem (runEnvironment readCode >>= compile)
        >> postcompile
        >> runOut

process :: (Dies es) => Code -> Eff es IR
process = lex >=> parse ast >=> analyse

compile :: (Dies es, FileSystem :> es) => Code -> Eff es ()
compile = process >=> dump

readCode :: (Environment :> es, Dies es, FileSystem :> es) => Eff es Code
readCode = getArgs >>= getPath >>= \p -> Code p <$> readFile p

getPath :: (Dies es) => [String] -> Eff es FilePath
getPath [] = die' expectedPath
getPath [path] = pure path
getPath (_ : a : _) = die' (unexpectedArgument a)

expectedPath :: String
expectedPath = argsError "expected argument"

argsError :: String -> String
argsError = (++) "! error reading args: "

unexpectedArgument :: String -> String
unexpectedArgument a = argsError ("unexpected argument: " ++ enclosed "`" a)

dump :: (FileSystem :> es) => IR -> Eff es ()
dump = writeFile outQbe . fromString . show

postcompile :: (IOE :> es) => Eff es ()
postcompile = qbe >> cc

outQbe, outS, out :: FilePath
outQbe = "out.qbe"
outS = "out.s"
out = "out"

qbe :: (IOE :> es) => Eff es ()
qbe = call "qbe" ["-o", outS, outQbe]

cc :: (IOE :> es) => Eff es ()
cc = call "cc" ["-o", out, outS]

runOut :: (IOE :> es) => Eff es ()
runOut = run ("./" ++ out) []
