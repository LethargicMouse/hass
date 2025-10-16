-- provides data types to describe Link `Expr` AST
module Link.AST.Expr (
  Expr (..),
  AtomExpr (..),
  FieldExpr (..),
  CallExpr (..),
  LetExpr (..),
  needsSemicolon,
  addPostfix,
) where

import Link.AST.Expr.Postfix (Postfix (..))
import Source.View (Viewed (..))

data AtomExpr
  = Unit
  | Let LetExpr
  | Call CallExpr
  | Var (Viewed String)
  | Str String
  | Int Integer

data FieldExpr
  = FieldExpr (Viewed Expr) (Viewed String)

data Expr
  = Atomic AtomExpr
  | Field FieldExpr

data CallExpr
  = CallExpr (Viewed String) [Expr]

data LetExpr
  = LetExpr String Expr

needsSemicolon :: Expr -> Bool
needsSemicolon _ = True

addPostfix :: Viewed Expr -> Viewed Postfix -> Viewed Expr
addPostfix e (Viewed v (FieldPostfix f)) =
  Viewed v $
    Field (FieldExpr e f)
