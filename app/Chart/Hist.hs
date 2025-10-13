module Chart.Hist (
  drawHist,
) where

import Draw (drawToFile)
import Graphics.Rendering.Chart.Easy

drawHist :: [Float] -> IO ()
drawHist = drawToFile "hist.svg" . histToPlot . hist

hist :: [x] -> PlotHist x Int
hist vs = defaultPlotHist{_plot_hist_values = vs}
