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
import Language.Trust.Program.Aliases (Aliases)
import Language.Trust.Program.Funs (Funs)
import Language.Trust.Program.Structs (Structs)

data Program
  = Program
  { _name :: String,
    _funs :: Funs,
    _structs :: Structs,
    _aliases :: Aliases
  }

makeLenses ''Program
