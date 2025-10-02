{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Link.Compiler.Generate.Stmts
  ( StmtsGen,
    stmts,
    addTmp,
    newStmtsGen,
  )
where

import Control.Lens (makeLenses)
import Control.Monad.State (MonadState, get, modify)
import qualified Qbe.Ir as IR
import Zoom (zoom)

data StmtsGen
  = StmtsGen
  { _stmts :: [IR.Stmt],
    _nextTmp :: Int
  }

makeLenses ''StmtsGen

addTmp :: (MonadState StmtsGen m) => m Int
addTmp = zoom nextTmp $ get <* modify (+ 1)

newStmtsGen :: StmtsGen
newStmtsGen =
  StmtsGen
    []
    1
