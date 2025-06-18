module Language.IR.Expr.Runner
  ( expr,
  )
where

import Control.Lens (view)
import Data.Map ((!))
import Language.IR.Expr (Expr (..))
import Language.IR.Fun (ret)
import Language.IR.Program (funs)
import Language.IR.Runner (Runner)

expr :: Expr -> Runner Expr
expr Unit = pure Unit
expr (Call n) = do
  fs <- view funs
  expr (ret $ fs ! n) -- TODO do something with this??
