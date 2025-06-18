module Language.Trust.Builder
  ( Builder,
    runBuilder,
  )
where

import Control.Monad.Except (Except, runExcept)
import Control.Monad.State (StateT, execStateT)
import Language.Trust.Builder.Error (Error)

type Builder a = StateT a (Except Error) ()

runBuilder :: Builder a -> a -> Either Error a
runBuilder b = runExcept . execStateT b
