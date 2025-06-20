{-# LANGUAGE TemplateHaskell #-}

module Language.Trust.Program
  ( Program (..),
    name,
    funs,
    structs,
  )
where

import Control.Lens (makeLenses)
import Data.Map (Map)
import Language.Trust.Fun (Fun)
import Language.Trust.Struct (Struct)

data Program
  = Program
  { _name :: String,
    _funs :: Map String Fun,
    _structs :: Map String Struct
  }

makeLenses ''Program
