{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Link.Lex where

import Combinators (($$), ($~), (?:))
import Data.ByteString.Char8 (ByteString, null, unpack)
import Data.List (scanl')
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError_)
import Effectful.NonDet (NonDet, empty)
import Effectful.Reader.Static (Reader, asks)
import Effectful.State.Static.Local (State, gets)
import Source (Source (..))
import Text (Render (..), Text, quote)
import Prelude hiding (null)

newtype Info = Info
  { name :: Text
  }

data Code = Code
  { code :: ByteString
  , poses :: [Pos]
  }

data Pos
  = Pos Int Int

type Location = Text

instance Render Pos where
  render (Pos l s) = render l <> ":" <> render s

data Lexeme = EOF

data Token = Token

lex :: (Error Text :> es) => Source -> Eff es [Token]
lex (Source n c) = lexer $$ Info n $~ Code c (getPoses c)

getPoses :: ByteString -> [Pos]
getPoses = scanl' nextPos (Pos 1 1) . unpack

nextPos :: Pos -> Char -> Pos
nextPos (Pos l _) '\n' = Pos (l + 1) 1
nextPos (Pos l s) _ = Pos l (s + 1)

lexer :: (Reader Info :> es, State Code :> es, Error Text :> es) => Eff es [Token]
lexer = do
  t <- next ?: failLex
  isNull <- gets (null . code)
  if isNull
    then pure [t]
    else (t :) <$> lexer

failLex :: (Reader Info :> es, State Code :> es, Error Text :> es) => Eff es a
failLex = locate >>= throwError_ . lexError

locate :: (Reader Info :> es, State Code :> es) => Eff es Location
locate = location <$> asks name <*> gets (head . poses)

location :: Text -> Pos -> Location
location n p = quote "`" n <> " at " <> render p

lexError :: Location -> Text
lexError l =
  "! error lexing "
    <> render l
    <> "\n--! unexpected token"

next :: (State Code :> es, NonDet :> es) => Eff es Token
next = empty

tok :: Lexeme -> Eff es Token
tok = const (pure Token)
