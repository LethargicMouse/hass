module String.Block (block) where

import String.Enclosed (enclosed)

block :: String -> String -> String
block n s =
  "\n\n"
    ++ enclosed (replicate 16 '-' ++ " ") n
    ++ "\n"
    ++ s
    ++ replicate (34 + length n) '-'
