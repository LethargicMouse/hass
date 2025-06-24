module Data'.List.NubSort.Ed
  ( nubSorted,
  )
where

nubSorted :: (Eq a) => [a] -> [a]
nubSorted (a : a' : as)
  | a == a' = nubSorted (a' : as)
  | otherwise = a : nubSorted (a' : as)
nubSorted as = as
