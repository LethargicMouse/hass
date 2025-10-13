module Link.Compiler.Args (
  Args (..),
  getArgs,
) where

import Die (dieOr)
import Link.Compiler.Args.Error (Error (..), ExpectedPath (..), UnexpectedArg (..))
import qualified System.Environment as E

getArgs :: IO Args
getArgs = dieOr . parse =<< E.getArgs

newtype Args
  = Args FilePath

parse :: [String] -> Either Error Args
parse [] = Left (EP ExpectedPath)
parse [p] = Right (Args p)
parse (_ : a : _) = Left (UA $ UnexpectedArg a)
