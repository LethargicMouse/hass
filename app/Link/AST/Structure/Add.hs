-- provides a generic function to add some item to a `Map`,
-- failing if an item with such name already exists
{-# LANGUAGE FlexibleContexts #-}

module Link.AST.Structure.Add (add) where

import Control.Lens ((^.))
import Control.Monad.Except (MonadError, throwError)
import Data.Map (Map, insert, (!?))
import Link.AST.Structure.Error (AlreadyExists (..))
import Named (Named (..))
import OfKind (OfKind (..))
import Source.View (HasView (..))

add ::
  ( Named a
  , MonadError AlreadyExists m
  , HasView a
  , OfKind a
  ) =>
  a ->
  Map String a ->
  m (Map String a)
add a m =
  let n = a ^. name
   in case m !? n of
        Nothing -> pure (insert n a m)
        Just a' ->
          throwError $
            AlreadyExists
              (a ^. view)
              (kind a)
              n
              (a' ^. view)
