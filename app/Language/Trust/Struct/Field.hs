module Language.Trust.Struct.Field
  ( Field (..),
  )
where

import Language.Trust.Type (Type)
import Language.View (View)

data Field
  = Field View Int Type
