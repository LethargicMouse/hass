module Autoregression (runAutoregression) where

import Autoregression.Csv (readCsv)
import Autoregression.Fit (epsilons, fit, takeFitPart)
import Chart.Draw (drawCharts)
import Chart.Hist (drawHist)
import Die (dieOr)

runAutoregression :: IO ()
runAutoregression = do
  vs <- readCsv
  let fs = takeFitPart vs
  cs <- dieOr (fit fs)
  let es = epsilons cs fs
  drawCharts [("predicted next", zip [1 ..] es), ("data", zip [1 ..] fs)]
  drawHist es
