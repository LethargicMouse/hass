module Source.Pos
  ( Pos (..),
    startPos,
    posify,
    nextPos,
  )
where

import Data.Hashable (Hashable (hashWithSalt))
import Data.List (scanl')

posify :: [Char] -> [Pos]
posify = scanl' nextPos startPos

startPos :: Pos
startPos = Pos 1 1

nextPos :: Pos -> Char -> Pos
nextPos (Pos l _) '\n' = Pos (l + 1) 1
nextPos (Pos l s) _ = Pos l (s + 1)

data Pos
  = Pos Int Int
  deriving (Eq, Ord)

instance Hashable Pos where
  hashWithSalt s (Pos a b) = s `hashWithSalt` a `hashWithSalt` b

instance Show Pos where
  show (Pos l s) = show l ++ ":" ++ show s
