{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Link.Lex where

import Code (Code (..))
import Data.ByteString.Char8 (ByteString, unpack)
import qualified Data.ByteString.Char8 as BS
import Data.List (scanl')
import Data.Vector (Vector, empty, fromList, (!))
import Effectful (Eff)
import Shorts (Dies, die, enclosed)

lex :: (Dies es) => Code -> Eff es [Token]
lex (Code n s) = lex' $ zip <$> (++ "\0") <*> poses $ unpack s
 where
  lex' [] = undefined
  lex' [(_, p)] = pure [Token Eof $ Location n p ls]
  lex' ((_, p) : _) = die (lexError $ Location n p ls)
  ls = fromList (BS.lines s)

lexError :: Location -> String
lexError l = "! error lexing " ++ show l ++ "\n--! unexpected token"

poses :: [Char] -> [Pos]
poses = scanl' nextPos startPos

nextPos :: Pos -> Char -> Pos
nextPos (Pos l _) '\n' = Pos (l + 1) 1
nextPos (Pos l s) _ = Pos l (s + 1)

data Token
  = Token
  { lexeme :: Lexeme
  , location :: Location
  }

data Location
  = Location
  { name :: String
  , start :: Pos
  , lines_ :: Vector ByteString
  }

instance Show Location where
  show (Location n s ls) =
    enclosed "`" n
      ++ " at "
      ++ show s
      ++ ":\n     |"
      ++ showLine (line s) ls
      ++ underline (symbol s) (symbol s + 1)

showLine :: Int -> Vector ByteString -> String
showLine n ls = '\n' : leftpad ' ' 4 (show n) ++ " | " ++ unpack (ls ! n)

underline :: Int -> Int -> String
underline a b = "\n     |" ++ replicate a ' ' ++ replicate (b - a) '`'

leftpad :: Char -> Int -> String -> String
leftpad c n s = replicate (n - length s) c ++ s

fakeLocation :: Location
fakeLocation = Location "<??>" startPos empty

data Pos
  = Pos {line :: Int, symbol :: Int}
  deriving (Eq, Ord)

instance Show Pos where
  show (Pos l s) = show l ++ ":" ++ show s

startPos :: Pos
startPos = Pos 1 1

data Lexeme
  = ParL
  | ParR
  | CurL
  | CurR
  | Name String
  | Int Int
  | Eof
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
