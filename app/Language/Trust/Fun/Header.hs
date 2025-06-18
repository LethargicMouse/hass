{-# LANGUAGE TemplateHaskell #-}

module Language.Trust.Fun.Header
  ( Header (..),
    name,
    nameView,
  )
where

import Control.Lens (makeLenses)
import Language.View (View)

data Header
  = Header
  { _nameView :: View,
    _name :: String
  }

makeLenses ''Header
