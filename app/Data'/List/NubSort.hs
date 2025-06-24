module Data'.List.NubSort
  ( nubSort,
  )
where

import Data'.List.NubSort.Ed (nubSorted)
import Data.List (sort)

nubSort :: (Ord a) => [a] -> [a]
nubSort = nubSorted . sort
