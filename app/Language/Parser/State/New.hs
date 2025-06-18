module Language.Parser.State.New (new) where

import Data.Vector (fromList)
import Language.Parser.State (State (..))
import Language.Source.Pos.Ify (posify)

new :: String -> String -> State
new n s =
  State
    (zip s ps)
    lp
    n
    (fromList $ lines s)
  where
    (lp, ps) = posify s
