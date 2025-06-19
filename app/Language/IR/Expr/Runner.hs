module Language.IR.Expr.Runner
  ( expr,
    set,
  )
where

import Control.Lens (modifying, use, view)
import Data.Map (insert, (!))
import Language.IR.Expr (Expr (..))
import Language.IR.Fun (ret)
import Language.IR.Program (funs)
import Language.IR.Runner (Runner)
import Language.IR.Runner.State (vars)
import Language.IR.Runner.State.Var (Var (..))

expr :: Expr -> Runner Expr
expr Unit = pure Unit
expr (Call n) = call n
expr (VarExpr n) = var n
expr (Block es e) = mapM_ expr es >> expr e
expr (Set s e) = Unit <$ set s e
expr (Str s) = pure (Str s)
expr (List es) = List <$> mapM expr es

call :: String -> Runner Expr
call n = do
  fs <- view funs
  expr (ret $ fs ! n) -- TODO do something with this??

var :: String -> Runner Expr
var n = do
  vs <- use vars
  let Var e = vs ! n
  expr e

set :: String -> Expr -> Runner ()
set s e = modifying vars . insert s . Var =<< expr e
