{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Link.Compiler.Generate.Stmts (
  ExprGen,
  stmts,
  addTmp,
  newStmtsGen,
  vars,
  consts,
  nextConst,
  addStr,
  addStmt,
)
where

import Control.Lens (makeLenses, modifying)
import Control.Monad.State (MonadState, get, modify)
import Data.Map (Map, empty)
import qualified Qbe.Ir as IR
import Zoom (zoom)

data ExprGen
  = ExprGen
  { _stmts :: [IR.Stmt]
  , _nextTmp :: Int
  , _vars :: Map String Int
  , _consts :: [String]
  , _nextConst :: Int
  }

makeLenses ''ExprGen

addTmp :: (MonadState ExprGen m) => m Int
addTmp = zoom nextTmp $ get <* modify (+ 1)

newStmtsGen :: ExprGen
newStmtsGen =
  ExprGen
    []
    1
    empty
    []
    0

addStr :: (MonadState ExprGen m) => String -> m Int
addStr s = do
  i <- zoom nextConst $ get <* modify (+ 1)
  i <$ modifying consts (s :)

addStmt :: (MonadState ExprGen m) => IR.Stmt -> m ()
addStmt = modifying stmts . (:)
