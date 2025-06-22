module Language.Trust.Program.Funs
  ( Funs,
  )
where

import Data.Map (Map)
import Language.Trust.Fun (Fun)

type Funs = Map String Fun
