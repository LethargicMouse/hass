module Language.Trust.Program.Aliases.Resolve
  ( resolve,
  )
where

import Control.Lens (view)
import Data.Map ((!?))
import Language.Trust.Alias (Alias (..))
import Language.Trust.Checker (Checker)
import Language.Trust.Program (aliases)
import Language.Trust.Type (Type (..))

resolve :: Type -> Checker Type
resolve (Name n) = do
  as <- view aliases
  case as !? n of
    Nothing -> pure (Name n)
    Just (Alias _ _ t) -> resolve t
resolve t = pure t
