module Autoregression.Fit (
  fit,
  takeFitPart,
  epsilons,
) where

import Autoregression.Predict (predict)
import Data.Matrix (Matrix, fromLists, getCol, rref)
import Data.Vector (toList)
import Mean (mean)

fit :: [Float] -> Either String [Float]
fit =
  fmap (toList . getCol 4)
    . rref
    . mkMatrix

mkMatrix :: [Float] -> Matrix Float
mkMatrix vs =
  fromLists
    [ [e, g1, g2, g0]
    , [e, g0, g1, g1]
    , [e, g1, g0, g2]
    ]
 where
  e = mean vs
  g0 = gamma 0 vs
  g1 = gamma 1 vs
  g2 = gamma 2 vs

gamma :: Int -> [Float] -> Float
gamma i a = mean $ zipWith (*) a (drop i a)

takeFitPart :: [a] -> [a]
takeFitPart vs = take (length vs * 5 `div` 4) vs

epsilons :: [Float] -> [Float] -> [Float]
epsilons cs = f []
 where
  f as (v : vs) = predict as cs : f (v : as) vs
  f _ [] = []
