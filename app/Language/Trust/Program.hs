{-# LANGUAGE TemplateHaskell #-}

module Language.Trust.Program
  ( Program (..),
    name,
    funs,
    structs,
    aliases,
  )
where

import Control.Lens (makeLenses)
import Data.Map (Map)
import Language.Trust.Alias (Alias)
import Language.Trust.Fun (Fun)
import Language.Trust.Struct (Struct)

data Program
  = Program
  { _name :: String,
    _funs :: Map String Fun,
    _structs :: Map String Struct,
    _aliases :: Map String Alias
  }

makeLenses ''Program
