{-# LANGUAGE TemplateHaskell #-}

module Link.Program.Info (
  Info (..),
  types,
  offsets,
  empty,
) where

import Control.Lens (makeLenses)
import Data.Map (Map)
import qualified Data.Map as M
import Link.AST (Type)

data Info
  = Info
  { _types :: Map Int Type
  , _offsets :: Map String (Map String Int)
  }
  deriving (Show)

makeLenses ''Info

empty :: Info
empty = Info M.empty M.empty
