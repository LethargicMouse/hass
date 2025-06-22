module Language.Trust.Program.Builder.Add.Struct
  ( addStruct,
  )
where

import Control.Lens (modifying, use)
import Control.Monad.Except (liftEither, throwError)
import Data.Map (empty, insert, (!?))
import Language.Trust.AST.Struct (Struct (..))
import Language.Trust.Builder (Builder)
import Language.Trust.Builder.Error (Error (..))
import Language.Trust.Builder.Error.AlreadyExists (AE (..))
import Language.Trust.Builder.Run (runBuilder)
import Language.Trust.Program (Program, structs)
import Language.Trust.Struct (nameView)
import qualified Language.Trust.Struct as S
import Language.Trust.Struct.Fields.Builder (fieldsBuilder)

addStruct :: Struct -> Builder Program
addStruct (Struct nv n fs) = do
  ss <- use structs
  case ss !? n of
    Nothing ->
      modifying structs
        . insert n
        . S.Struct nv
        =<< liftEither (runBuilder (fieldsBuilder fs) empty)
    Just s ->
      throwError $
        AlreadyExists
          ( AE
              nv
              "struct"
              n
              (nameView s)
          )
