-- provides data type to describe Link `Expr` `Postfix` AST
module Link.AST.Expr.Postfix (Postfix (..)) where

import Source.View (Viewed)

newtype Postfix
  = FieldPostfix (Viewed String)
