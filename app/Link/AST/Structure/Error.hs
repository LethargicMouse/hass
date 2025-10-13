-- provides an error that `structure` may throw
module Link.AST.Structure.Error (
  AlreadyExists (..),
) where

import Enclosed (enclosed)
import Source.View (View)

data AlreadyExists
  = AlreadyExists View String String View

instance Show AlreadyExists where
  show (AlreadyExists v n k v') =
    "! error "
      ++ show v
      ++ "\n--! "
      ++ k
      ++ enclosed " `" n
      ++ "is already declared "
      ++ show v'
