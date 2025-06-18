module Language.Trust.Program.Checker.Main
  ( main,
  )
where

import Control.Lens (view)
import Control.Monad (void)
import Data.Map ((!?))
import Language.Trust.Checker (Checker)
import Language.Trust.Checker.Error (Error (..))
import Language.Trust.Checker.Util.OrFail (orFail)
import Language.Trust.Program (funs, name)

main :: Checker ()
main = do
  fs <- view funs
  n <- view name
  void $
    (fs !? "main")
      `orFail` NoMain n
