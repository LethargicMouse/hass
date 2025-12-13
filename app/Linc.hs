{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Command (call)
import qualified Command as C
import Control.Monad (void)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Effect.Exit (Exit, die, runExit)
import Effect.File (File, runFile)
import Effect.Stdio (Stdio, runStdio)
import Effectful (Eff, IOE, runEff, runPureEff, (:>))
import Effectful.Environment (Environment, runEnvironment)
import Effectful.Error.Static (Error, runError, runErrorNoCallStack)
import Effectful.Process (Process, runProcess)
import Effectful.State.Static.Local (State, evalState, get)
import Linc.Args (Args (..), Command (..), getArgs, liftError)
import Qbe.IR (IR (..), dump)
import Source (Source (..), readSource)
import Prelude hiding (lex)

main :: IO ()
main = runEffs $ do
  Args c <- getArgs
  case c of
    Clean -> clean
    Run path -> run path

run :: (Error Builder :> es, File :> es, Process :> es, Exit :> es) => FilePath -> Eff es ()
run p =
  readSource p
    >>= compile
    >>= dump
    >> postCompile
    >> runOut

clean :: (Process :> es) => Eff es ()
clean = void $ runError @Builder $ call "rm" ["out.qbe", "out.s", "out"]

runEffs :: Eff '[Error Builder, Stdio, Exit, Environment, File, Process, IOE] a -> IO a
runEffs =
  runEff . runProcess . runFile . runEnvironment . runExit . runStdio . die

runOut :: (Process :> es, Exit :> es) => Eff es ()
runOut = C.run "./out" []

compile :: (Error Builder :> es) => Source -> Eff es IR
compile = fmap (generate . analyse . parse) . lex

lex :: (Error Builder :> es) => Source -> Eff es [Token]
lex (Source c) = evalState c lexer

lexer :: (State ByteString :> es, Error Builder :> es) => Eff es [Token]
lexer = do
  t <- next
  s <- get
  if B.null s
    then pure [t]
    else (t :) <$> lexer

next :: (State ByteString :> es, Error Builder :> es) => Eff es Token
next = _

tok :: Lexeme -> Eff es Token
tok _ = pure Token

data Lexeme = EOF

parse :: [Token] -> AST
parse _ = AST

analyse :: AST -> ASG
analyse _ = ASG

generate :: ASG -> IR
generate _ = IR

data ASG = ASG

data AST = AST

data Token = Token

postCompile :: (Process :> es, Error Builder :> es) => Eff es ()
postCompile = do
  call "qbe" ["-o", "out.s", "out.qbe"]
  call "cc" ["-o", "out", "out.s"]
