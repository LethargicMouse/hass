{-# LANGUAGE TemplateHaskell #-}

module Language.Parser.Error
  ( Error (..),
    view,
    messages,
  )
where

import qualified Control.Lens as L
import Data'.List.NubSort (nubSort)
import Data.Function (on)
import Data.Vector (empty)
import Language.Source.Pos (Pos (..))
import Language.View (View (..), start)

data Error
  = Error
  { _view :: View,
    _messages :: [String]
  }

L.makeLenses ''Error

instance Show Error where
  show (Error v msgs) =
    "! error "
      ++ show v
      ++ "\n--! expected:"
      ++ concatMap ("\n    - " ++) (nubSort msgs)

instance Semigroup Error where
  a <> b = case on compare (L.view $ view . start) a b of
    LT -> b
    GT -> a
    EQ -> L.over messages (++ L.view messages a) b

instance Monoid Error where
  mempty =
    Error
      (View "<unknown>" (Pos 0 1) (Pos 0 1) empty)
      []
