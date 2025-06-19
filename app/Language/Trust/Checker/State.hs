{-# LANGUAGE TemplateHaskell #-}

module Language.Trust.Checker.State
  ( State (..),
    vars,
  )
where

import Control.Lens (makeLenses)
import Data.Map (Map)
import Language.Trust.Checker.State.Var (Var)

newtype State
  = State
  { _vars :: Map String Var
  }

makeLenses ''State
