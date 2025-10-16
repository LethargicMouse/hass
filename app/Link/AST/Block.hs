-- provides data type to describe Link `Block` AST
module Link.AST.Block (Block (..)) where

import Link.AST.Expr (Expr)

data Block
  = Block [Expr] Expr
