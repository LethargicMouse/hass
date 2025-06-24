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
expr (Get e i) = get <$> expr e <*> pure i
expr (Int i) = pure (Int i)
expr (Not a) = not' <$> expr a
expr (Bool a) = pure (Bool a)
expr (Equal a b) = equal <$> expr a <*> expr b
expr (If c t e) = expr c >>= expr . if' t e

if' :: Expr -> Expr -> Expr -> Expr
if' t e (Bool b) = if b then t else e
if' _ _ e = error ("runner failed: `" ++ show e ++ "` is not boolean")

equal :: Expr -> Expr -> Expr
equal a b = Bool (a == b)

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

not' :: Expr -> Expr
not' (Bool b) = Bool (not b)
not' e = error ("runner failed: unable to negate `" ++ show e ++ "`")
