module Language.IR.Program.Runner
  ( program,
  )
where

import Control.Monad (void)
import Control.Monad.Reader (ask)
import Data.Map ((!))
import Language.IR.Expr (Expr (..))
import Language.IR.Expr.Runner (expr, set)
import Language.IR.Fun (ret)
import Language.IR.Program (Program (..))
import Language.IR.Runner (Runner)

program :: [String] -> Runner ()
program args = do
  _ <- set "args" $ List [Int $ toInteger $ length args]
  Program funs <- ask
  void $ expr (ret $ funs ! "main")
