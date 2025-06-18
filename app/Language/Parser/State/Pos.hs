module Language.Parser.State.Pos
  ( pos,
  )
where

import Language.Parser.State (State (..))
import Language.Source.Pos (Pos)

pos :: State -> Pos
pos (State rest lp _ _) = case rest of
  [] -> lp
  (_, p) : _ -> p
