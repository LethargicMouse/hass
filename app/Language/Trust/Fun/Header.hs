{-# LANGUAGE TemplateHaskell #-}

module Language.Trust.Fun.Header
  ( Header (..),
    name,
  )
where

import Control.Lens (makeLenses)

newtype Header
  = Header
  { _name :: String
  }

makeLenses ''Header
