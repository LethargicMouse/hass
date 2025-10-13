module Main (main) where

import Autoregression (runAutoregression)
import Excel.Pro (runExcelPro)
import qualified Link.Compiler
import Script (runScript)

toRun :: ToRun
toRun = LinkCompiler

data ToRun
  = LinkCompiler
  | Autoregression
  | ExcelPro
  | Script

main :: IO ()
main = case toRun of
  Autoregression -> runAutoregression
  LinkCompiler -> Link.Compiler.run
  ExcelPro -> runExcelPro
  Script -> runScript
