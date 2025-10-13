module BaseFix.ReadFile (readFile) where

import Control.Exception (IOException, try)
import Die (die)
import Enclosed (enclosed)
import System.IO.Error (ioeGetErrorString)
import Prelude hiding (readFile)
import qualified Prelude as P

readFile :: FilePath -> IO String
readFile p =
  either (die . Error p) pure
    =<< try (P.readFile p)

data Error
  = Error FilePath IOException

instance Show Error where
  show (Error p e) =
    "! error reading "
      ++ enclosed "`" p
      ++ ": "
      ++ ioeGetErrorString e
