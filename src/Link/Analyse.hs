{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Link.Analyse where

import Control.Lens (makeLenses, view)
import DList (DList, append, dList, toList)
import Effectful (Eff, (:>))
import Effectful.State.Static.Local (State, execState)
import Link.AST (AST (..), Call (..), Expr (..))
import Qbe.IR (IR (..))
import Qbe.IR.Stmt (Arg (..), Copy (..), Stmt (..))
import qualified Qbe.IR.Stmt as IR
import Qbe.IR.Type (AbiType (T), Type (..))
import Shorts (assign, modifying, use)

data StmtGen
  = StmtGen
  { _stmts :: DList Stmt
  , _nextTmp :: Int
  }

makeLenses ''StmtGen

analyse :: AST -> Eff es IR
analyse (AST e) = IR . result <$> execState stmtGen (expr e)

expr :: (State StmtGen :> es) => Expr -> Eff es Int
expr Unit = unit
expr (Int n) = int n
expr (CallExpr c) = call c

call :: (State StmtGen :> es) => Call -> Eff es Int
call (Call n e) = do
  a <- expr e
  i <- newTmp
  i <$ add (Ca $ IR.Call i (T Word) n [Arg (T Long) a])

unit :: (State StmtGen :> es) => Eff es Int
unit = int 0

int :: (State StmtGen :> es) => Int -> Eff es Int
int n = newTmp >>= \i -> i <$ add (Co $ Copy i Long n)

newTmp :: (State StmtGen :> es) => Eff es Int
newTmp = use nextTmp >>= \t -> t <$ assign nextTmp (t + 1)

add :: (State StmtGen :> es) => Stmt -> Eff es ()
add = modifying stmts . append

stmtGen :: StmtGen
stmtGen = StmtGen (dList []) 1

result :: StmtGen -> [Stmt]
result = toList . view stmts
