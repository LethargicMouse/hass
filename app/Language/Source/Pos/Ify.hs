module Language.Source.Pos.Ify
  ( posify,
  )
where

import Language.Source.Pos (Pos (..))

posify :: String -> (Pos, [Pos])
posify = aux (Pos 1 1)
  where
    aux p [] = (p, [])
    aux p@(Pos l _) ('\n' : cs) = (p :) <$> aux (Pos (l + 1) 1) cs
    aux p@(Pos l s) (_ : cs) = (p :) <$> aux (Pos l $ s + 1) cs
