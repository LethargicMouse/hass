module Run.Process.Fail (Fail (..)) where

import String.Block (block)
import String.Enclosed (enclosed)

data Fail
  = -- name, exit code, stdout, stderr
  Fail
  { name :: String,
    code :: Int,
    stdout :: String,
    stderr :: String
  }

instance Show Fail where
  show (Fail n c o e) =
    "! error running "
      ++ enclosed "`" n
      ++ ":\n--! process failed with exit code "
      ++ show c
      ++ block "stdout" o
      ++ block "stderr" e
