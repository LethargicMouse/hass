module Language.Trust.AST.Top
  ( Top (..),
  )
where

import Language.Trust.AST.Struct (Struct)
import Language.Trust.Alias (Alias)
import Language.Trust.Fun (Fun)

data Top
  = FunTop Fun
  | StructTop Struct
  | AliasTop Alias
