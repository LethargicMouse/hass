module Language.Trust.Program.Builder.Add
  ( add,
  )
where

import Control.Lens (zoom)
import Language.Trust.AST.Top (Top (..))
import Language.Trust.Builder (Builder)
import Language.Trust.Program (Program, aliases, funs, structs)
import Language.Trust.Program.Builder.Add.Alias (addAlias)
import Language.Trust.Program.Builder.Add.Fun (addFun)
import Language.Trust.Program.Builder.Add.Struct (addStruct)

add :: Top -> Builder Program
add (FunTop f) = zoom funs (addFun f)
add (StructTop s) = zoom structs (addStruct s)
add (AliasTop a) = zoom aliases (addAlias a)
