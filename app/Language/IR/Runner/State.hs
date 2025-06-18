{-# LANGUAGE TemplateHaskell #-}

module Language.IR.Runner.State
  ( State (..),
    vars,
  )
where

import Control.Lens (makeLenses)
import Data.Map (Map)
import Language.IR.Runner.State.Var (Var)

newtype State
  = State
  { _vars :: Map String Var
  }

makeLenses ''State
