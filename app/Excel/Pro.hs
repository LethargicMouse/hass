{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Excel.Pro (runExcelPro) where

import Codec.Xlsx
import Control.Lens
import qualified Data.ByteString.Lazy as L
import Mean (mean)

runExcelPro :: IO ()
runExcelPro = do
  bs <- L.readFile "data.xlsx"
  let table =
        toXlsx
          bs
          ^?! ixSheet "Sheet1"
  putStrLn $ "суммарная площадь: " ++ show (sum $ col 4 table)
  putStrLn $ "средняя ставка аренды: " ++ show (mean $ col 5 table)
  putStrLn $ "суммарный приход по аренде: " ++ show (sum $ col 6 table)

val :: Int -> Int -> Worksheet -> Double
val i j ws =
  let CellDouble v = ws ^?! ixCell (fromIntegral i, fromIntegral j) . cellValue . _Just
   in v

col :: Int -> Worksheet -> [Double]
col n ws = [val i n ws | i <- [2 .. cnt ws]]

cnt :: Worksheet -> Int
cnt = round . val 2 10
