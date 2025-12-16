{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Link.Lex where

import Combinators (($$), ($~), (?:))
import Data.ByteString.Char8 (ByteString, isPrefixOf, length, null, takeWhile, unpack)
import qualified Data.ByteString.Char8 as B
import Data.Char (isSpace)
import Data.List (scanl')
import Data.String (IsString (fromString))
import Data.Vector (Vector, (!))
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError_)
import Effectful.NonDet (NonDet, empty, (<|>))
import Effectful.Reader.Static (Reader, asks)
import Effectful.State.Static.Local (State, gets, modify)
import Source (Info (..), Source (..))
import Text (Render (..), Text, leftpad, quote)
import Prelude hiding (length, null, takeWhile)

data Code = Code
  { code :: ByteString
  , poses :: [Pos]
  }

data Pos
  = Pos Int Int

type Location = Text

instance Render Pos where
  render (Pos l s) = render l <> ":" <> render s

data Lexeme
  = EOF
  | Fn

data Token = Token

lex :: (Error Text :> es) => Source -> Eff es [Token]
lex (Source i c) = lexer $$ i $~ Code c (getPoses c)

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
failLex = throwError_ . lexError =<< locate

locate :: (Reader Info :> es, State Code :> es) => Eff es Location
locate = location <$> asks name <*> gets (head . poses) <*> asks codeLines

location :: Text -> Pos -> Vector Text -> Location
location n p@(Pos l s) ls =
  quote "`" n
    <> " at "
    <> render p
    <> ":\n     |"
    <> line l (ls ! pred l)
    <> underline s (s + 1)

line :: Int -> Text -> Text
line n s =
  "\n"
    <> leftpad 4 (fromString $ show n)
    <> " | "
    <> s

underline :: Int -> Int -> Text
underline s e =
  "\n     |"
    <> render (replicate s ' ')
    <> render (replicate (e - s) '`')

lexError :: Location -> Text
lexError l =
  "! error lexing "
    <> render l
    <> "\n--! unexpected token"

next :: (State Code :> es, NonDet :> es) => Eff es Token
next = skip >> fromList lexList

skip :: (State Code :> es) => Eff es ()
skip = consume . length . takeWhile isSpace =<< gets code

lexList :: [(ByteString, Lexeme)]
lexList = [("fn", Fn)]

fromList :: (NonDet :> es, State Code :> es) => [(ByteString, Lexeme)] -> Eff es Token
fromList [] = empty
fromList ((s, l) : o) =
  do
    c <- gets code
    if s `isPrefixOf` c
      then consume (length s) >> tok l
      else empty
    <|> fromList o

consume :: (State Code :> es) => Int -> Eff es ()
consume n = modify $ \(Code s ps) -> Code (B.drop n s) (drop n ps)

tok :: Lexeme -> Eff es Token
tok = const (pure Token)
