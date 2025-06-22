module Language.Trust.AST.Struct
  ( Struct (..),
  )
where

import Language.Trust.AST.Field (Field)
import Language.View (View)

data Struct
  = Struct View String [Field]
