module Language.Parser.Util.View
  ( view,
  )
where

import Control.Lens (use)
import Data.Vector (slice)
import Language.Parser (Parser)
import Language.Parser.State (codeLines, srcName)
import Language.Source.Pos (Pos (..))
import Language.View (View (..))

view :: Pos -> Pos -> Parser View
view s@(Pos sl _) e@(Pos el _) = do
  n <- use srcName
  ls <- slice (sl - 1) (el - sl + 1) <$> use codeLines
  pure (View n s e ls)
