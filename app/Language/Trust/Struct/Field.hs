module Language.Trust.Struct.Field
  ( Field (..),
  )
where

import Language.Trust.Type (Type)

data Field
  = Field Int Type
