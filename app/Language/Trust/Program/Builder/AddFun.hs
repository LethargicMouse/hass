module Language.Trust.Program.Builder.AddFun
  ( addFun,
  )
where

import Control.Lens (modifying, use, view)
import Control.Monad.Except (throwError)
import Data.Map (insert, (!?))
import Language.Trust.Builder (Builder)
import Language.Trust.Builder.Error (Error (..))
import Language.Trust.Builder.Error.AlreadyExists (AE (..))
import Language.Trust.Fun (Fun, header)
import Language.Trust.Fun.Header (name, nameView)
import Language.Trust.Program (Program, funs)

addFun :: Fun -> Builder Program
addFun f = do
  let n = view (header . name) f
  fs <- use funs
  case fs !? n of
    Nothing -> modifying funs (insert n f)
    Just f' ->
      throwError $
        AlreadyExists
          ( AE
              (view (header . nameView) f)
              "functon"
              n
              (view (header . nameView) f')
          )
