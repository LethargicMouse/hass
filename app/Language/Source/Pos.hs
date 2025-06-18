module Language.Source.Pos
  ( Pos (..),
  )
where

data Pos = Pos Int Int
  deriving (Eq, Ord)

instance Show Pos where
  show (Pos l s) = show l ++ ":" ++ show s
