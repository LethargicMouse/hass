{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Command (call)
import qualified Command as C
import Control.Monad (void)
import Data.ByteString.Char8 (ByteString, unpack)
import qualified Data.ByteString.Char8 as B
import Data.List (scanl')
import Effect.Exit (Exit, die, runExit)
import Effect.File (File, runFile)
import Effect.Stdio (Stdio, runStdio)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Environment (Environment, runEnvironment)
import Effectful.Error.Static (Error, runError, throwError_)
import Effectful.NonDet (NonDet, OnEmptyPolicy (OnEmptyKeep), empty, runNonDet)
import Effectful.Process (Process, runProcess)
import Effectful.Reader.Static (Reader, asks, runReader)
import Effectful.State.Static.Local (State, evalState, gets)
import Linc.Args (Args (..), Command (..), getArgs)
import Qbe.IR (IR (..), dump)
import Source (Source (..), readSource)
import Text (Render (..), Text, quote, text)
import Prelude hiding (lex)

main :: IO ()
main = runEffs $ do
  Args c <- getArgs
  case c of
    Clean -> clean
    Run path -> run path

run :: (Error Text :> es, File :> es, Process :> es, Exit :> es) => FilePath -> Eff es ()
run p =
  readSource p
    >>= compile
    >>= dump
    >> postCompile
    >> runOut

clean :: (Process :> es) => Eff es ()
clean = void $ runError @Text $ call "rm" ["out.qbe", "out.s", "out"]

runEffs :: Eff '[Error Text, Stdio, Exit, Environment, File, Process, IOE] a -> IO a
runEffs =
  runEff . runProcess . runFile . runEnvironment . runExit . runStdio . die

runOut :: (Process :> es, Exit :> es) => Eff es ()
runOut = C.run "./out" []

compile :: (Error Text :> es) => Source -> Eff es IR
compile = fmap (generate . analyse . parse) . lex

lex :: (Error Text :> es) => Source -> Eff es [Token]
lex (Source n c) =
  runReader (Info n) $
    evalState (Code c ps) lexer
 where
  ps = getPoses c

getPoses :: ByteString -> [Pos]
getPoses = scanl' nextPos (Pos 1 1) . unpack

nextPos :: Pos -> Char -> Pos
nextPos (Pos l _) '\n' = Pos (l + 1) 1
nextPos (Pos l s) _ = Pos l (s + 1)

lexer :: (Reader Info :> es, State Code :> es, Error Text :> es) => Eff es [Token]
lexer = do
  t <- detOr failLex next
  isNull <- gets (B.null . code)
  if isNull
    then pure [t]
    else (t :) <$> lexer

detOr :: Eff es a -> Eff (NonDet : es) a -> Eff es a
detOr a b = runNonDet OnEmptyKeep b >>= either (const a) pure

failLex :: (Reader Info :> es, State Code :> es, Error Text :> es) => Eff es a
failLex = locate >>= throwError_ . lexError

locate :: (Reader Info :> es, State Code :> es) => Eff es Location
locate = location <$> asks name <*> gets (head . poses)

newtype Info = Info
  { name :: Text
  }

data Code = Code
  { code :: ByteString
  , poses :: [Pos]
  }

location :: Text -> Pos -> Location
location n p = quote "`" n <> " at " <> render p

data Pos
  = Pos Int Int

instance Render Pos where
  render (Pos l s) = render l <> ":" <> render s

renderPos :: Pos -> Text
renderPos (Pos l s) = text l <> ":" <> text s

lexError :: Location -> Text
lexError l = "! error lexing " <> l <> "\n--! unexpected token"

type Location = Text

next :: (State Code :> es, NonDet :> es) => Eff es Token
next = empty

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

postCompile :: (Process :> es, Error Text :> es) => Eff es ()
postCompile = do
  call "qbe" ["-o", "out.s", "out.qbe"]
  call "cc" ["-o", "out", "out.s"]
