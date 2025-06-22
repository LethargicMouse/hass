module Language.Trust.Struct.Fields.Builder.Add
  ( add,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.State (get, modify)
import Data.Map (insert, (!?))
import Language.Trust.AST.Field (Field (..))
import Language.Trust.Builder (Builder)
import Language.Trust.Builder.Error (Error (..))
import Language.Trust.Builder.Error.AlreadyExists (AE (..))
import qualified Language.Trust.Struct.Field as F
import Language.Trust.Struct.Fields (Fields)

add :: Int -> Field -> Builder Fields
add i (Field nv n t) = do
  fs <- get
  case fs !? n of
    Nothing ->
      modify $
        insert n (F.Field nv i t)
    Just (F.Field v _ _) ->
      throwError $
        AlreadyExists
          ( AE
              nv
              "field"
              n
              v
          )
