-- provides functions to access parts of `Info`
{-# LANGUAGE FlexibleContexts #-}

module Source.Info.Getters where

import Control.Monad.Reader (MonadReader, asks)
import Source (Info (..))
import Source.Pos (Pos, nextPos)
import Source.View (View (View))

viewAt :: (MonadReader Info m) => Pos -> m View
viewAt p = viewBetween p (nextPos p ' ')

viewBetween :: (MonadReader Info m) => Pos -> Pos -> m View
viewBetween s e = do
  n <- asks srcName
  ls <- asks codeLines
  pure $ View n s e ls
