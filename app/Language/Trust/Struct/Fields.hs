module Language.Trust.Struct.Fields
  ( Fields,
  )
where

import Data.Map (Map)
import Language.Trust.Struct.Field (Field)

type Fields = Map String Field
