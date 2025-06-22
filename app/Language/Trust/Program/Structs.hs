module Language.Trust.Program.Structs
  ( Structs,
  )
where

import Data.Map (Map)
import Language.Trust.Struct (Struct)

type Structs = Map String Struct
