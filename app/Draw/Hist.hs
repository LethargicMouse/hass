module Draw.Hist (drawHist) where

import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy

drawHist :: [Float] -> IO ()
drawHist vs = toFile def "hist.svg" $ do
  plot $
    pure $
      histToPlot
        defaultPlotHist
          { _plot_hist_values = vs
          }
