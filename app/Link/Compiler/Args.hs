module Link.Compiler.Args (Args (..), getArgs) where

import Die (dieOr)
import String.Enclosed (enclosed)
import qualified System.Environment as E

getArgs :: IO Args
getArgs = dieOr . parse =<< E.getArgs

newtype Args
  = Args FilePath

parse :: [String] -> Either Error Args
parse [] = Left (EP ExpectedPath)
parse [p] = Right (Args p)
parse (_ : a : _) = Left (UA $ UnexpectedArg a)

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
