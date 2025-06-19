{-# LANGUAGE TemplateHaskell #-}

module Language.IR.Program
  ( Program (..),
    funs,
  )
where

import Control.Lens (makeLenses)
import Data.Map (Map)
import Language.IR.Fun (Fun)

newtype Program
  = Program
  { _funs :: Map String Fun
  }
  deriving (Show)

makeLenses ''Program
