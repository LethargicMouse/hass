module Language.Trust.Builder.Run
  ( runBuilder,
  )
where

import Control.Monad.Except (runExcept)
import Control.Monad.State (execStateT)
import Language.Trust.Builder (Builder)
import Language.Trust.Builder.Error (Error)

runBuilder :: Builder a -> a -> Either Error a
runBuilder b = runExcept . execStateT b
