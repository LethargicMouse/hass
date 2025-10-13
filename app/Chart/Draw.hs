module Chart.Draw (
  drawChart,
  drawCharts,
) where

import Draw (draw, drawToFile)
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy

drawChart :: String -> [(Int, Float)] -> IO ()
drawChart n vs = drawToFile "chart.svg" (n, vs)

drawCharts :: [(String, [(Int, Float)])] -> IO ()
drawCharts cs = toFile def "chart.svg" (mapM_ draw cs)
