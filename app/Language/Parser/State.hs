{-# LANGUAGE TemplateHaskell #-}

module Language.Parser.State
  ( State (..),
    rest,
    lastPos,
    srcName,
    codeLines,
  )
where

import Control.Lens (makeLenses)
import Data.Vector (Vector)
import Language.Source.Pos (Pos)

data State
  = State
  { _rest :: [(Char, Pos)],
    _lastPos :: Pos,
    _srcName :: String,
    _codeLines :: Vector String
  }

makeLenses ''State
