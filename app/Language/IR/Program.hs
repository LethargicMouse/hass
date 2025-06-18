module Language.IR.Program
  ( Program (..),
  )
where

import Data.Map (Map)
import Language.IR.Fun (Fun)

newtype Program
  = Program (Map String Fun)
