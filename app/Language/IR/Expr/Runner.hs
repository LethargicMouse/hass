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
import qualified Language.IR.Runner.State.Var as V

expr :: Expr -> Runner Expr
expr Unit = pure Unit
expr (Call n) = call n
expr (VarExpr n) = var n
expr (Block es e) = mapM_ expr es >> expr e
expr (Set s e) = Unit <$ set s e
expr (Str s) = pure (Str s)
expr (List es) = List <$> mapM expr es
expr (Get e i) = do
  e' <- expr e
  pure (get e' i)
expr (Int i) = pure (Int i)

call :: String -> Runner Expr
call n = do
  fs <- view funs
  expr (ret $ fs ! n) -- TODO do something with this??

var :: String -> Runner Expr
var n = do
  vs <- use vars
  -- don't need to eval var value, as it is already eval'ed
  pure $ view V.expr (vs ! n)

set :: String -> Expr -> Runner ()
set s e = modifying vars . insert s . Var =<< expr e

get :: Expr -> Int -> Expr
get (List es) i = es !! i
get e _ = error ("runner failed: unable to index `" ++ show e ++ "`")
