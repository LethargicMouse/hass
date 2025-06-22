module Language.Trust.Program.Builder.Add.Alias
  ( addAlias,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.State (get, modify)
import Data.Map (insert, (!?))
import Language.Trust.Alias (Alias (..))
import Language.Trust.Builder (Builder)
import Language.Trust.Builder.Error (Error (..))
import Language.Trust.Builder.Error.AlreadyExists (AE (..))
import Language.Trust.Program.Aliases (Aliases)

addAlias :: Alias -> Builder Aliases
addAlias a@(Alias nv n _) = do
  as <- get
  case as !? n of
    Nothing ->
      modify (insert n a)
    Just (Alias nv' _ _) ->
      throwError $
        AlreadyExists
          ( AE
              nv
              "type alias"
              n
              nv'
          )
