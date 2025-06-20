{-# LANGUAGE TemplateHaskell #-}

module Language.IR.Runner.State.Var
  ( Var (..),
    expr,
  )
where

import Control.Lens (makeLenses)
import Language.IR.Expr (Expr)

newtype Var
  = Var
  { _expr :: Expr
  }

makeLenses ''Var
