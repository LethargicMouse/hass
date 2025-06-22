module Language.Trust.Program.Builder.Add.Struct
  ( addStruct,
  )
where

import Control.Monad.Except (liftEither, throwError)
import Control.Monad.State (get, modify)
import Data.Map (empty, insert, (!?))
import Language.Trust.AST.Struct (Struct (..))
import Language.Trust.Builder (Builder)
import Language.Trust.Builder.Error (Error (..))
import Language.Trust.Builder.Error.AlreadyExists (AE (..))
import Language.Trust.Builder.Run (runBuilder)
import Language.Trust.Program.Structs (Structs)
import Language.Trust.Struct (nameView)
import qualified Language.Trust.Struct as S
import Language.Trust.Struct.Fields.Builder (fieldsBuilder)

addStruct :: Struct -> Builder Structs
addStruct (Struct nv n fs) = do
  ss <- get
  case ss !? n of
    Nothing ->
      modify
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
