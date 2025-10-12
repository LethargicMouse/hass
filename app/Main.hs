module Main (main) where

import Autoregression (runAutoregression)
import Excel.Pro (runExcelPro)
import Link.Compiler (runLinkCompiler)
import Script (runScript)

toRun :: ToRun
toRun = ExcelPro

data ToRun
  = LinkCompiler
  | Autoregression
  | ExcelPro
  | Script

main :: IO ()
main = case toRun of
  Autoregression -> runAutoregression
  LinkCompiler -> runLinkCompiler
  ExcelPro -> runExcelPro
  Script -> runScript
