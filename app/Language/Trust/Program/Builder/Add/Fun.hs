module Language.Trust.Program.Builder.Add.Fun
  ( addFun,
  )
where

import Control.Lens (view)
import Control.Monad.Except (throwError)
import Control.Monad.State (get, modify)
import Data.Map (insert, (!?))
import Language.Trust.Builder (Builder)
import Language.Trust.Builder.Error (Error (..))
import Language.Trust.Builder.Error.AlreadyExists (AE (..))
import Language.Trust.Fun (Fun, header)
import Language.Trust.Fun.Header (name, nameView)
import Language.Trust.Program.Funs (Funs)

addFun :: Fun -> Builder Funs
addFun f = do
  let n = view (header . name) f
  fs <- get
  case fs !? n of
    Nothing -> modify (insert n f)
    Just f' ->
      throwError $
        AlreadyExists
          ( AE
              (view (header . nameView) f)
              "functon"
              n
              (view (header . nameView) f')
          )
