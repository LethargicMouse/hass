module Language.Trust
  ( main,
  )
where

import Language.Trust.Args.Process (process)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= process
