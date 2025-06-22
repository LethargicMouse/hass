{-# LANGUAGE TemplateHaskell #-}

module Language.Trust.AST.Field
  ( Field (..),
    name,
    type',
    nameView,
  )
where

import Control.Lens (makeLenses)
import Language.Trust.Type (Type)
import Language.View (View)

data Field
  = Field
  { _nameView :: View,
    _name :: String,
    _type' :: Type
  }

makeLenses ''Field
