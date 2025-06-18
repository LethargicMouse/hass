{-# LANGUAGE TemplateHaskell #-}

module Language.View
  ( View (..),
    srcName,
    start,
    end,
    slice,
  )
where

import Control.Lens
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Language.Source.Pos (Pos (..))

data View
  = View
  { _srcName :: String,
    _start :: Pos,
    _end :: Pos,
    _slice :: Vector String
  }

makeLenses ''View

instance Show View where
  show (View n s e ls) =
    "in "
      ++ n
      ++ " at "
      ++ show s
      ++ ":\n     | "
      ++ slice' s e ls

-- TODO move them all in their own modules

slice' :: Pos -> Pos -> Vector String -> String
slice' (Pos sl ss) (Pos el es) ls =
  case V.length ls of
    0 -> ""
    1 -> line sl l1 ++ underline ss es
    _ ->
      line sl l1
        ++ underline ss (length l1)
        ++ concatMap (line <*> (ls !)) [sl ..]
        ++ line el le
        ++ underline 1 es
  where
    l1 = V.head ls
    le = V.last ls

underline :: Int -> Int -> String
underline s e =
  "\n     |"
    ++ replicate s ' '
    ++ replicate (e - s + 1) '`'

line :: Int -> String -> String
line i s = '\n' : leftPad 4 ' ' (show i) ++ " | " ++ s

leftPad :: Int -> Char -> String -> String
leftPad n c s = replicate (n - length s) c ++ s
