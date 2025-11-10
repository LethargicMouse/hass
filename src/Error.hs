module Error where

import System.Exit (exitFailure)

die :: (Show e) => e -> IO a
die e = print e >> exitFailure
