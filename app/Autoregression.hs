module Autoregression (runAutoregression) where

import Autoregression.Csv (readCsv)
import Autoregression.Predict (predict)
import Data.Matrix (Matrix, fromLists, getCol, rref)
import Data.Vector (toList)
import Die (dieOr)
import Draw.Chart (drawCharts)
import Draw.Hist (drawHist)
import Mean (mean)

runAutoregression :: IO ()
runAutoregression = do
  vs <- readCsv
  let sz = length vs
  let fsz = sz * 5 `div` 4
  let fs = take fsz vs
  let m = mkMatrix fs
  m' <- dieOr (rref m) -- solve
  let cs = toList (getCol 4 m')
  let es = epsilons cs fs
  drawCharts [("predicted next", zip [1 ..] es), ("data", zip [1 ..] vs)]
  drawHist es

mkMatrix :: [Float] -> Matrix Float
mkMatrix vs =
  fromLists
    [ [e, g1, g2, g0],
      [e, g0, g1, g1],
      [e, g1, g0, g2]
    ]
  where
    e = mean vs
    g0 = gamma 0 vs
    g1 = gamma 1 vs
    g2 = gamma 2 vs

epsilons :: [Float] -> [Float] -> [Float]
epsilons cs = f []
  where
    f as (v : vs) = predict as cs : f (v : as) vs
    f _ [] = []

gamma :: Int -> [Float] -> Float
gamma i a = mean $ zipWith (*) a (drop i a)
