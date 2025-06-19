{-# LANGUAGE TemplateHaskell #-}

module Language.Trust.Fun.Header
  ( Header (..),
    name,
    nameView,
    params,
  )
where

import Control.Lens (makeLenses)
import Language.Trust.AST.Field (Field)
import Language.View (View)

data Header
  = Header
  { _nameView :: View,
    _name :: String,
    _params :: [Field]
  }

makeLenses ''Header
