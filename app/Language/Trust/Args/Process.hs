module Language.Trust.Args.Process
  ( process,
  )
where

import Display.Message (Message (..))
import Language.Trust.Run.File (runFile)
import Process.Die (die)

process :: [String] -> IO ()
process s = case s of
  [p] -> runFile p
  _ -> die (Message "! expected exactly one argument")
