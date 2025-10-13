module Process (
  run,
) where

import Control.Exception (try)
import Die (die)
import Run.Process.Error (Error (..))
import Run.Process.Fail (Fail (..))
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode, showCommandForUser)

run :: String -> [String] -> IO ()
run n as = do
  let n' = showCommandForUser n as
  (c, o, e) <-
    either (die . Error n') pure
      =<< try (readProcessWithExitCode n as "")
  case c of
    ExitSuccess -> pure ()
    ExitFailure c' -> die (Fail n' c' o e)
