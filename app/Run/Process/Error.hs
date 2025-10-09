module Run.Process.Error (Error (..)) where

import Control.Exception (IOException)
import String.Enclosed (enclosed)
import System.IO.Error (ioeGetErrorString)

data Error
  = Error String IOException

instance Show Error where
  show (Error n e) =
    "! failed to run "
      ++ enclosed "`" n
      ++ ": "
      ++ ioeGetErrorString e
