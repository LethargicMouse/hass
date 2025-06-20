{-# LANGUAGE TemplateHaskell #-}

module Language.Trust.AST.Field
  ( Field (..),
    name,
    type',
  )
where

import Control.Lens (makeLenses)
import Language.Trust.Type (Type)

data Field
  = Field
  { _name :: String,
    _type' :: Type
  }

makeLenses ''Field
