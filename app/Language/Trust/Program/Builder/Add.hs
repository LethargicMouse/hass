module Language.Trust.Program.Builder.Add
  ( add,
  )
where

import Language.Trust.AST.Top (Top (..))
import Language.Trust.Builder (Builder)
import Language.Trust.Program (Program)
import Language.Trust.Program.Builder.Add.Alias (addAlias)
import Language.Trust.Program.Builder.Add.Struct (addStruct)
import Language.Trust.Program.Builder.AddFun (addFun)

add :: Top -> Builder Program
add (FunTop f) = addFun f
add (StructTop s) = addStruct s
add (AliasTop a) = addAlias a
