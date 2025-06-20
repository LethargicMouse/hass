module Language.Trust.Struct
  ( Struct (..),
  )
where

import Data.Map (Map)
import Language.Trust.Struct.Field (Field)

newtype Struct
  = Struct
  { fields :: Map String Field
  }
