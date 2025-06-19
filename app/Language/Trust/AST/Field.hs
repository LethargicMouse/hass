{-# LANGUAGE TemplateHaskell #-}

module Language.Trust.AST.Field
  ( Field (..),
    name,
  )
where

import Control.Lens (makeLenses)

newtype Field
  = Field
  { _name :: String
  }

makeLenses ''Field
