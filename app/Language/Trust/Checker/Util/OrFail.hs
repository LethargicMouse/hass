module Language.Trust.Checker.Util.OrFail
  ( orFail,
  )
where

import Control.Monad.Except (throwError)
import Language.Trust.Checker (Checker)
import Language.Trust.Checker.Error (Error)

infixl 1 `orFail`

orFail :: Maybe a -> Error -> Checker a
orFail ma e = maybe (throwError e) pure ma
