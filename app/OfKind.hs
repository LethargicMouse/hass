module OfKind where

class OfKind a where
  kind :: a -> String
