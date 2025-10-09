module Script (runScript) where

import Data.List.Split (splitOn)
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy (Default (def), plot, points)

runScript :: IO ()
runScript = do
  d <- readData
  let ps = zipWith (curry $ fmap fpnr) [1 :: Int ..]
      fs = ps $ filter (\x -> species x == 5) d
      rs = ps $ filter (\x -> species x /= 5) d
  toFile def "scatter.svg" $ do
    plot (points "#5" fs)
    plot (points "rest" rs)

readData :: IO [Record]
readData = parseData <$> readFile "lizards.csv"

parseData :: String -> [Record]
parseData = map parseRecord . drop 1 . lines

parseRecord :: String -> Record
parseRecord s =
  Record
    { species = read (head ws),
      fpnr = read (ws !! 7)
    }
  where
    ws = splitOn "," s

data Record
  = Record
  { species :: Int,
    fpnr :: Float
  }
