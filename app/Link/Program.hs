{-# LANGUAGE TemplateHaskell #-}

module Link.Program (
  Program (..),
  empty,
  name,
  items,
  typeItems,
  TypeItem (..),
  Struct (Struct),
  fields,
) where

import Control.Lens (makeLenses)
import Data.Map (Map)
import qualified Data.Map as M
import Link.AST (Item, Type)

data Program
  = Program
  { _name :: String
  , _items :: Map String Item
  , _typeItems :: Map String TypeItem
  }

newtype TypeItem
  = StructItem Struct

newtype Struct
  = Struct
  { _fields :: Map String Type
  }

makeLenses ''Program
makeLenses ''Struct

empty :: String -> Program
empty n = Program n M.empty M.empty
