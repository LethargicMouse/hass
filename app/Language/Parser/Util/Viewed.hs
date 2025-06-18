module Language.Parser.Util.Viewed
  ( viewed,
    viewed',
  )
where

import Control.Monad.State (gets)
import Language.Parser (Parser)
import Language.Parser.State.Pos (pos)
import Language.Parser.Util.SkipSpace (skipSpace)
import Language.Parser.Util.View (view)
import Language.View (View)

viewed :: Parser a -> Parser (View, a)
viewed p = skipSpace >> viewed' p

viewed' :: Parser a -> Parser (View, a)
viewed' p = do
  s <- gets pos
  res <- p
  e <- gets pos
  v <- view s e
  pure (v, res)
