module Link.Program (Program (..), empty) where

import Data.Map (Map)
import qualified Data.Map as M
import Link.AST (Item)

data Program
  = Program
  { name :: String,
    items :: Map String Item
  }

empty :: String -> Program
empty n = Program n M.empty
