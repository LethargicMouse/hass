module Language.Trust.Type.Elem
  ( elemType,
  )
where

import Language.Trust.Checker (Checker)
import Language.Trust.Type (Type)

elemType :: Type -> Checker Type
elemType t =
  error
    ( "fatal check error: type `"
        ++ show t
        ++ "` has no element, and it will be enforced later with classes"
    )
