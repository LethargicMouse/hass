module Language.Trust.Program.Builder
  ( programBuilder,
  )
where

import Control.Lens (assign, modifying, view)
import Data.Map (insert)
import Language.Trust.AST (AST (..))
import Language.Trust.Builder (Builder)
import Language.Trust.Fun (header)
import qualified Language.Trust.Fun.Header as H
import Language.Trust.Program (Program, funs, name)

programBuilder :: AST -> Builder Program
programBuilder (AST n f) = do
  assign name n
  modifying funs $
    insert (view (header . H.name) f) f
