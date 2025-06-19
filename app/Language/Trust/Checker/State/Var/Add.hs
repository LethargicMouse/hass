module Language.Trust.Checker.State.Var.Add
  ( add,
  )
where

import Control.Lens (modifying)
import Data.Map (insert)
import Language.Trust.AST.Field (Field (..))
import Language.Trust.Checker (Checker)
import Language.Trust.Checker.State (vars)
import Language.Trust.Checker.State.Var (Var (..))

add :: Field -> Checker ()
add (Field n) =
  modifying vars $
    insert n Var
