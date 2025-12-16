{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Link.Lex where

import Combinators (($$), ($~), (?:))
import Control.Monad (guard)
import Data.ByteString.Char8 (ByteString, isPrefixOf, length, null, takeWhile, unpack)
import qualified Data.ByteString.Char8 as B
import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (scanl', singleton)
import Data.String (IsString (fromString))
import Data.Vector (Vector, (!))
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError_)
import Effectful.NonDet (NonDet, empty, many, (<|>))
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
  | Name ByteString
  | ParL
  | ParR
  | CurL
  | CurR

data Token = Token

lex :: (Error Text :> es) => Source -> Eff es [Token]
lex (Source i c) = lexer $$ i $~ Code c (getPoses c)

getPoses :: ByteString -> [Pos]
getPoses = scanl' nextPos (Pos 1 1) . unpack

nextPos :: Pos -> Char -> Pos
nextPos (Pos l _) '\n' = Pos (l + 1) 1
nextPos (Pos l s) _ = Pos l (s + 1)

lexer :: (Reader Info :> es, State Code :> es, Error Text :> es) => Eff es [Token]
lexer = ((++) <$> many next <*> (singleton <$> eof)) ?: failLex

eof :: (State Code :> es, NonDet :> es) => Eff es Token
eof = do
  guard . null =<< gets code
  tok EOF

failLex :: (Reader Info :> es, State Code :> es, Error Text :> es) => Eff es a
failLex = throwError_ . lexError =<< locate

locate :: (Reader Info :> es, State Code :> es) => Eff es Location
locate = location <$> asks sourceName <*> gets (head . poses) <*> asks codeLines

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
next =
  skip
    >> fromList lexList
      <|> name

name :: (State Code :> es, NonDet :> es) => Eff es Token
name = do
  res <- gets (takeWhile isNameChar . code)
  guard (not $ null res)
  guard (isNameFirstChar $ B.head res)
  consume (length res)
  tok (Name res)

isNameChar :: Char -> Bool
isNameChar c = isNameFirstChar c || isDigit c

isNameFirstChar :: Char -> Bool
isNameFirstChar c = isAlpha c || c == '_'

skip :: (State Code :> es) => Eff es ()
skip = consume . length . takeWhile isSpace =<< gets code

lexList :: [(ByteString, Lexeme)]
lexList =
  [ ("fn", Fn)
  , ("(", ParL)
  , (")", ParR)
  , ("{", CurL)
  , ("}", CurR)
  ]

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
