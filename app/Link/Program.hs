{-# LANGUAGE TemplateHaskell #-}

module Link.Program (
  Program (..),
  empty,
  name,
  items,
) where

import Control.Lens (makeLenses)
import Data.Map (Map)
import qualified Data.Map as M
import Link.AST (Item)

data Program
  = Program
  { _name :: String
  , _items :: Map String Item
  }

makeLenses ''Program

empty :: String -> Program
empty n = Program n M.empty
