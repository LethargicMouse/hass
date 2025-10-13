module Source.Parse.Error (
  Error (..),
) where

import Data.Function (on)
import Data.Set (Set, empty)
import Source.View (View, fakeView, start)

data Error = Error
  { messages :: Set String
  , view :: View
  }

instance Show Error where
  show (Error ms v) =
    "! parse error "
      ++ show v
      ++ "\n--! expected:"
      ++ concatMap ("\n    - " ++) ms

instance Semigroup Error where
  a@(Error ams av) <> b@(Error bms bv) = case (compare `on` start) av bv of
    LT -> b
    GT -> a
    EQ ->
      let v = if length ams > length bms then av else bv
       in Error (ams <> bms) v

instance Monoid Error where
  mempty = Error empty fakeView
