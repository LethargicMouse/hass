module Language.Trust.Fun.Header.Checker
  ( header,
  )
where

import Control.Lens (set, view)
import Control.Monad (forM_)
import Language.Trust.AST.Field (type')
import Language.Trust.Checker (Checker)
import Language.Trust.Checker.State.Var.Add (add)
import Language.Trust.Fun.Header (Header (..))
import Language.Trust.Program.Aliases.Resolve (resolve)

header :: Header -> Checker ()
header (Header _ _ params _) = do
  forM_ params $ \f -> do
    t' <- resolve (view type' f)
    add (set type' t' f)
