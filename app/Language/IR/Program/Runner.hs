module Language.IR.Program.Runner
  ( program,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Map ((!))
import Language.IR.Expr (Expr (..))
import Language.IR.Expr.Runner (expr)
import Language.IR.Fun.Runner (fun)
import Language.IR.Program (Program (..))
import Language.IR.Runner (Runner)
import System.Environment (getArgs)

program :: Runner ()
program = do
  args <- liftIO getArgs
  _ <- expr $ Set "args" $ List (Str <$> args)
  Program funs <- ask
  void $ fun (funs ! "main")
