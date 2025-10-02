module String.Enclosed (enclosed) where

enclosed :: String -> String -> String
enclosed c s = c ++ s ++ reverse c
