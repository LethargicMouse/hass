{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Code (Code, getCode)
import Control.Monad ((>=>))
import Data.String (IsString (fromString))
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Console.ByteString (Console)
import Effectful.Environment (Environment, getArgs, runEnvironment)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.FileSystem.IO.ByteString (writeFile)
import Linc.Args (getPath)
import Linc.Post (outQbe, postcompile, runOut)
import Link.Analyse (analyse)
import Link.Lex (lex)
import Link.Parse (ast, parse)
import Qbe.IR (IR)
import Shorts (Dies, Exit, runDeath)
import Prelude hiding (lex, readFile, writeFile)

main :: IO ()
main = runApp $ readCode >>= compile >> postcompile >> runOut

runApp :: Eff '[Console, Exit, FileSystem, Environment, IOE] a -> IO a
runApp = runEff . runEnvironment . runFileSystem . runDeath

process :: (Dies es) => Code -> Eff es IR
process = lex >=> parse ast >=> analyse

compile :: (Dies es, FileSystem :> es) => Code -> Eff es ()
compile = process >=> dump

readCode :: (Environment :> es, Dies es, FileSystem :> es) => Eff es Code
readCode = getArgs >>= getPath >>= getCode

dump :: (FileSystem :> es) => IR -> Eff es ()
dump = writeFile outQbe . fromString . show
