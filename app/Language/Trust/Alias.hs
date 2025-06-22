module Language.Trust.Alias
  ( Alias (..),
  )
where

import Language.Trust.Type (Type)
import Language.View (View)

data Alias
  = Alias View String Type
