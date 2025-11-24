{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad ((>=>))
import Data.String (IsString (fromString))
import Effectful (Eff, runEff, (:>))
import Effectful.Environment (Environment, getArgs, runEnvironment)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.FileSystem.IO.ByteString (readFile, writeFile)
import Linc.Args (getPath)
import Linc.Post (outQbe, postcompile, runOut)
import Link.Analyse (analyse)
import Link.Lex (Code (..), lex)
import Link.Parse (ast, parse)
import Qbe.IR (IR)
import Shorts (Dies, runDeath)
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

dump :: (FileSystem :> es) => IR -> Eff es ()
dump = writeFile outQbe . fromString . show
