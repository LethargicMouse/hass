{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Link.Lex where

import Code (Code (..))
import Data.ByteString.Char8 (isPrefixOf, unpack)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace)
import Data.Vector (fromList)
import Effectful (Eff)
import Location (Location (..), poses)
import Shorts (Dies, die, enclosed)

lex :: (Dies es) => Code -> Eff es [Token]
lex (Code n s) = lex' <*> poses . unpack $ s
 where
  lex' a' ps'
    | BS.null a = pure [Token Eof loc]
    | has "fn" = (Token Fn loc :) <$> lex' (BS.drop 2 a) (drop 2 ps)
    | otherwise = die (lexError loc)
   where
    spaces = BS.length $ BS.takeWhile isSpace a'
    a = BS.drop spaces a'
    ps = drop spaces ps'
    loc = Location n (head ps) ls
    has s' = s' `isPrefixOf` a
  ls = fromList (BS.lines s)

lexError :: Location -> String
lexError l = "! error lexing " ++ show l ++ "\n--! unexpected token"

data Token
  = Token
  { lexeme :: Lexeme
  , location :: Location
  }

data Lexeme
  = ParL
  | ParR
  | CurL
  | CurR
  | Name String
  | Int Int
  | Eof
  | Fn
  deriving (Eq)

instance Show Lexeme where
  show Eof = "<eof>"
  show l = enclosed "`" $ case l of
    ParL -> "("
    ParR -> ")"
    CurL -> "{"
    CurR -> "}"
    Name n -> n
    Int i -> show i
    Fn -> "fn"
