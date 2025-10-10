{-# LANGUAGE TemplateHaskell #-}

module Source.View (
  View (..),
  fakeView,
  HasView (..),
  Viewed (Viewed),
  unwrap,
)
where

import Control.Lens (Lens', makeLenses)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Vector (Vector, empty, (!))
import Source.Pos (Pos (..), startPos)
import String.Enclosed (enclosed)

data View
  = View
  { srcName :: String
  , start :: Pos
  , end :: Pos
  , code :: Vector String
  }

instance Eq View where
  a == b = start a == start b && end a == end b && srcName a == srcName b

instance Hashable View where
  hashWithSalt s v =
    s `hashWithSalt` start v `hashWithSalt` end v

instance Show View where
  show (View n s e ls) =
    "in "
      ++ enclosed "`" n
      ++ " at "
      ++ show s
      ++ ":\n     |"
      ++ showCode s e ls

showCode :: Pos -> Pos -> Vector String -> String
showCode (Pos sl ss) (Pos el es) ls
  | sl == el = line sl l1 ++ underline ss es
  | otherwise =
      line sl l1
        ++ underline ss (length l1)
        ++ concatMap (\i -> line i $ ls ! (i - 1)) [sl + 1 .. el]
        ++ underline 1 es
 where
  l1 = ls ! (sl - 1)

underline :: Int -> Int -> String
underline a b = "\n     |" ++ replicate a ' ' ++ replicate (b - a) '`'

line :: Int -> String -> String
line n s = "\n" ++ leftpad 4 (show n) ++ " | " ++ s

leftpad :: Int -> String -> [Char]
leftpad n s = replicate (n - length s) ' ' ++ s

fakeView :: View
fakeView = View "<unknown>" startPos startPos empty

class HasView a where
  view :: Lens' a View

data Viewed a = Viewed {_view_ :: View, _unwrap :: a}

makeLenses ''Viewed

instance HasView (Viewed a) where
  view = view_

instance Functor Viewed where
  fmap f (Viewed v a) = Viewed v (f a)
