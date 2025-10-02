module Draw.Chart (drawChart, drawCharts) where

import Control.Monad (forM_)
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy

drawChart :: String -> [(Int, Float)] -> IO ()
drawChart n vs = drawCharts [(n, vs)]

drawCharts :: [(String, [(Int, Float)])] -> IO ()
drawCharts cs = toFile def "chart.svg" $ do
  forM_ cs $ \(n, c) -> plot (line n [c])
