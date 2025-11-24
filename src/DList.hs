{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module DList where

newtype DList a = DList {unDList :: [a] -> [a]}

toList :: DList a -> [a]
toList (DList l) = l []

instance Semigroup (DList a) where
  DList a <> DList b = DList $ a . b

dList :: [a] -> DList a
dList = DList . (++)

append :: a -> DList a -> DList a
append a l = l <> dList [a]
