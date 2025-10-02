module Autoregression.Predict (predict, predicts) where

predict :: [Float] -> [Float] -> Float
predict as cs = sum $ zipWith (*) (1 : as) cs

predicts :: Int -> [Float] -> [Float] -> [Float]
predicts 0 _ _ = []
predicts n as cs = p : predicts (n - 1) (p : as) cs
  where
    p = predict as cs
