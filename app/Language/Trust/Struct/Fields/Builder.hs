module Language.Trust.Struct.Fields.Builder
  ( fieldsBuilder,
  )
where

import Language.Trust.AST.Field (Field)
import Language.Trust.Builder (Builder)
import Language.Trust.Struct.Fields (Fields)
import Language.Trust.Struct.Fields.Builder.Add (add)

fieldsBuilder :: [Field] -> Builder Fields
fieldsBuilder = mapM_ (uncurry add) . zip [0 ..]
