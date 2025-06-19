module Language.Trust.Fun.Header.Checker
  ( header,
  )
where

import Control.Monad (forM_)
import Language.Trust.Checker (Checker)
import Language.Trust.Checker.State.Var.Add (add)
import Language.Trust.Fun.Header (Header (..))

header :: Header -> Checker ()
header (Header _ _ params) = do
  forM_ params add
