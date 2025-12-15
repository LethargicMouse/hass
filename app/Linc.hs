{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Combinators ((!:), (.>), (<.>))
import Command (call)
import qualified Command as C
import Control.Monad ((<=<))
import Effect.Exit (Exit, die, runExit)
import Effect.File (File, runFile)
import Effect.Stdio (Stdio, runStdio)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Environment (Environment, runEnvironment)
import Effectful.Error.Static (Error)
import Effectful.Process (Process, runProcess)
import Linc.Args (Args (..), Command (..), getArgs)
import Link.Lex (Token, lex)
import Qbe.IR (IR (..), dump)
import Source (Source (..), readSource)
import Text (Text)
import Prelude hiding (lex)

data ASG = ASG

data AST = AST

main :: IO ()
main =
  runEffs $
    \case
      Clean -> clean
      Run path -> run path
      . command
      =<< getArgs

run :: (Error Text :> es, File :> es, Process :> es, Exit :> es) => FilePath -> Eff es ()
run =
  (dump <=< compile <=< readSource)
    .> postCompile
    .> runOut

clean :: (Process :> es) => Eff es ()
clean = ignoreError @Text $ call "rm" ["out.qbe", "out.s", "out"]

ignoreError :: Eff (Error e : es) () -> Eff es ()
ignoreError m = m !: pure ()

runEffs :: Eff '[Error Text, Stdio, Exit, Environment, File, Process, IOE] a -> IO a
runEffs =
  runEff . runProcess . runFile . runEnvironment . runExit . runStdio . die

runOut :: (Process :> es, Exit :> es) => Eff es ()
runOut = C.run "./out" []

compile :: (Error Text :> es) => Source -> Eff es IR
compile = generate . analyse . parse <.> lex

parse :: [Token] -> AST
parse _ = AST

analyse :: AST -> ASG
analyse _ = ASG

generate :: ASG -> IR
generate _ = IR

postCompile :: (Process :> es, Error Text :> es) => Eff es ()
postCompile = do
  call "qbe" ["-o", "out.s", "out.qbe"]
  call "cc" ["-o", "out", "out.s"]
