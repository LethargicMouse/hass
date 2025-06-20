module Language.Trust.Struct.Field.Find
  ( findField,
  )
where

import Control.Lens (view)
import Data.Map ((!?))
import Language.Trust.Checker (Checker)
import Language.Trust.Checker.Error (Error (..))
import Language.Trust.Checker.Error.NoField (NF (..))
import Language.Trust.Checker.Util.OrFail (orFail)
import Language.Trust.Program (structs)
import Language.Trust.Struct (Struct (..))
import Language.Trust.Struct.Field (Field)
import Language.Trust.Type (Type)
import Language.View (View)

findField :: Type -> View -> String -> Checker Field
findField n mv m = do
  ss <- view structs
  (ss !? show n)
    >>= (!? m)
      . fields
      `orFail` NoField
        (NF mv m n)
