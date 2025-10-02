module Main (main) where

import Autoregression (runAutoregression)
import Excel.Pro (runExcelPro)
import Link.Compiler (runLinkCompiler)

toRun :: ToRun
toRun = LinkCompiler

data ToRun
  = LinkCompiler
  | Autoregression
  | ExcelPro

main :: IO ()
main = case toRun of
  Autoregression -> runAutoregression
  LinkCompiler -> runLinkCompiler
  ExcelPro -> runExcelPro
