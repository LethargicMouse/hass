module Run.Process (runProcess) where

import Control.Exception (try)
import Die (die)
import Run.Process.Error (Error (..))
import Run.Process.Fail (Fail (..))
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode, showCommandForUser)

runProcess :: String -> [String] -> IO ()
runProcess n as = do
  let cfu = showCommandForUser n as
  (c, o, e) <-
    either (die . Error cfu) pure
      =<< try (readProcessWithExitCode n as "")
  case c of
    ExitSuccess -> pure ()
    ExitFailure c' ->
      die
        Fail
          { name = cfu,
            code = c',
            stdout = o,
            stderr = e
          }
