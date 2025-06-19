module Language.Parser.State.New (new) where

import Data.Vector (fromList)
import Language.Parser.State (State (..))
import Language.Source.Pos.Ify (posify)

new :: String -> String -> State
new n s =
  State
    { _rest = zip s ps,
      _lastPos = lp,
      _srcName = n,
      _codeLines = fromList $ lines s
    }
  where
    (lp, ps) = posify s
