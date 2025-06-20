{-# LANGUAGE TemplateHaskell #-}

module Language.Trust.Fun.Header
  ( Header (..),
    name,
    nameView,
    params,
    retType,
  )
where

import Control.Lens (makeLenses)
import Language.Trust.AST.Field (Field)
import Language.Trust.Type (Type)
import Language.View (View)

data Header
  = Header
  { _nameView :: View,
    _name :: String,
    _params :: [Field],
    _retType :: Type
  }

makeLenses ''Header
