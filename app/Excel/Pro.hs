{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Excel.Pro (runExcelPro) where

import Codec.Xlsx
import Control.Lens
import qualified Data.ByteString.Lazy as L
import Die (dieOr)
import Mean (mean)

redFont :: Font
redFont = def & fontColor ?~ (def & colorARGB ?~ "FFFF0000")

addRedFont :: StyleSheet -> (StyleSheet, Int)
addRedFont s =
  let fs = s ^. styleSheetFonts
      fs' = fs ++ [redFont]
      i = length fs
   in (s & styleSheetFonts .~ fs', i)

cellFormat :: Int -> CellXf
cellFormat i =
  def
    & cellXfFontId ?~ i
    & cellXfApplyFont ?~ True

runExcelPro :: IO ()
runExcelPro = do
  bs <- L.readFile "data.xlsx"
  let xl = toXlsx bs
  let t = xl ^?! ixSheet "Sheet1"
  putStrLn $ "суммарная площадь: " ++ show (sum $ col 4 t)
  putStrLn $ "средняя ставка аренды: " ++ show (mean $ col 5 t)
  putStrLn $ "суммарный приход по аренде: " ++ show (sum $ col 6 t)

val :: Int -> Int -> Worksheet -> Double
val i j ws =
  let CellDouble v = ws ^?! ixCell (fromIntegral i, fromIntegral j) . cellValue . _Just
   in v

col :: Int -> Worksheet -> [Double]
col n ws = [val i n ws | i <- [2 .. cnt ws]]

cnt :: Worksheet -> Int
cnt = round . val 2 10
