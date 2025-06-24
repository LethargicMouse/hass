module Language.Trust.Expr
  ( Block (..),
    Expr (..),
    Call (..),
    Var (..),
    Field (..),
    BinExpr (..),
    If (..),
  )
where

import Language.Trust.AST.Expr.Binary (Binary)
import Language.Trust.Expr.Literal (Literal)
import Language.View (View)

data Expr
  = Literal Literal
  | CallExpr Call
  | FieldExpr Field
  | VarExpr Var
  | BinaryExpr BinExpr
  | IfExpr If
  | BlockExpr Block

newtype Block
  = Block Expr

data Call
  = Call View String [Expr]

data Var
  = Var View String

data Field
  = Field Expr View String

data BinExpr
  = BinExpr Expr Binary Expr

data If
  = If Expr Expr Expr
