module Language.Trust.Checker.Util.OrFail
  ( orFail,
  )
where

import Control.Monad.Except (throwError)
import Language.Trust.Checker (Checker)
import Language.Trust.Checker.Error (Error)

orFail :: Maybe a -> Error -> Checker a
orFail ma e = maybe (throwError e) pure ma
