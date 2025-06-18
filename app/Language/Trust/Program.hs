{-# LANGUAGE TemplateHaskell #-}

module Language.Trust.Program
  ( Program (..),
    name,
    funs,
  )
where

import Control.Lens (makeLenses)
import Data.Map (Map)
import Language.Trust.Fun (Fun)

data Program
  = Program
  { _name :: String,
    _funs :: Map String Fun
  }

makeLenses ''Program
