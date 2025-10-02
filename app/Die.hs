module Die (die, dieOr) where

import System.Exit (exitFailure)

die :: (Show e) => e -> IO a
die e = do
  print e
  exitFailure

dieOr :: (Show e) => Either e a -> IO a
dieOr = either die pure
