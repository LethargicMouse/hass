module Location where

import Data.ByteString.Char8 (ByteString, unpack)
import Data.List (scanl')
import Data.Vector (Vector, empty, fromList, (!))
import Shorts (enclosed)

poses :: String -> Vector Pos
poses = fromList . scanl' nextPos startPos

nextPos :: Pos -> Char -> Pos
nextPos (Pos l _) '\n' = Pos (l + 1) 1
nextPos (Pos l s) _ = Pos l (s + 1)

data Pos
  = Pos {line :: Int, symbol :: Int}
  deriving (Eq, Ord)

instance Show Pos where
  show (Pos l s) = show l ++ ":" ++ show s

startPos :: Pos
startPos = Pos 1 1

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
