module Link.Compiler.Args.Error (
  Error (..),
  ExpectedPath (..),
  UnexpectedArg (..),
) where

import Enclosed (enclosed)

data Error
  = EP ExpectedPath
  | UA UnexpectedArg

instance Show Error where
  show e' =
    "! error in args: " ++ case e' of
      EP e -> show e
      UA e -> show e

data ExpectedPath
  = ExpectedPath

instance Show ExpectedPath where
  show ExpectedPath = "expected path"

newtype UnexpectedArg
  = UnexpectedArg String

instance Show UnexpectedArg where
  show (UnexpectedArg a) = "unexpected arg: " ++ enclosed "`" a
