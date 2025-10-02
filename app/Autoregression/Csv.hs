module Autoregression.Csv (readCsv) where

import Data.List.Split (splitOn)

readCsv :: IO [Float]
readCsv = parseCsv <$> readFile "data.csv"

parseCsv :: String -> [Float]
parseCsv = map parseRecord . drop 1 . lines

parseRecord :: String -> Float
parseRecord s = cv
  where
    ws = splitOn "," s
    cv = read (ws !! 5)
