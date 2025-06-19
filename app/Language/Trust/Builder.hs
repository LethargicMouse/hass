module Language.Trust.Builder
  ( Builder,
  )
where

import Control.Monad.Except (Except)
import Control.Monad.State (StateT)
import Language.Trust.Builder.Error (Error)

type Builder a = StateT a (Except Error) ()
