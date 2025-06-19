module Language.IR.Program.Runner
  ( program,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Map ((!))
import Language.IR.Expr (Expr (..))
import Language.IR.Expr.Runner (expr, set)
import Language.IR.Fun (ret)
import Language.IR.Program (Program (..))
import Language.IR.Runner (Runner)
import System.Environment (getArgs)

program :: Runner ()
program = do
  args <- liftIO getArgs
  _ <- set "args" $ List (Str <$> args)
  Program funs <- ask
  void $ expr (ret $ funs ! "main")
