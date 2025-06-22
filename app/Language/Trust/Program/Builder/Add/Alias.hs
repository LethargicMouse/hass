module Language.Trust.Program.Builder.Add.Alias
  ( addAlias,
  )
where

import Control.Lens (modifying, use)
import Control.Monad.Except (throwError)
import Data.Map (insert, (!?))
import Language.Trust.Alias (Alias (..))
import Language.Trust.Builder (Builder)
import Language.Trust.Builder.Error (Error (..))
import Language.Trust.Builder.Error.AlreadyExists (AE (..))
import Language.Trust.Program (Program, aliases)

addAlias :: Alias -> Builder Program
addAlias a@(Alias nv n _) = do
  as <- use aliases
  case as !? n of
    Nothing ->
      modifying aliases (insert n a)
    Just (Alias nv' _ _) ->
      throwError $
        AlreadyExists
          ( AE
              nv
              "type alias"
              n
              nv'
          )
