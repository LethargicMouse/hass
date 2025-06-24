module Language.Trust.Type.Elem
  ( elemType,
  )
where

import Language.Trust.Type (Type (..))

-- tmp, will be useless with `get` class
elemType :: Type -> Type
elemType (Ref t) = t
elemType t =
  error
    ( "fatal check error: type `"
        ++ show t
        ++ "` can't be indexed, and it will be enforced later with classes"
    )
