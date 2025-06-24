module Language.Trust.Args.Process
  ( process,
  )
where

import Display.Message (Message (..))
import Language.Trust.Run.File (runFile)
import Process.Die (die)

process :: [String] -> IO ()
process s = case s of
  p : args -> runFile p args
  [] -> die (Message "! expected path to file")
