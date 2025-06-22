module Language.Trust.Struct
  ( Struct (..),
  )
where

import Language.Trust.Struct.Fields (Fields)
import Language.View (View)

data Struct
  = Struct
  { nameView :: View,
    fields :: Fields
  }
