module Language.Trust.Fun.Header.Checker.Find
  ( findHeader,
  )
where

import Control.Lens (view)
import Data.Map ((!?))
import Language.Trust.Checker (Checker)
import Language.Trust.Checker.Error (Error (..))
import Language.Trust.Checker.Error.NotDeclared (ND (..))
import Language.Trust.Checker.Util.OrFail (orFail)
import Language.Trust.Fun (header)
import Language.Trust.Fun.Header (Header)
import Language.Trust.Program (funs)
import Language.View (View)

findHeader :: String -> View -> Checker Header
findHeader n v = do
  fs <- view funs
  view header
    <$> (fs !? n)
      `orFail` NotDeclared
        (ND v "function" n)
