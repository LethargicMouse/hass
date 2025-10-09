module Link.AST (
  AST (..),
  Item (..),
  Fn (..),
  Header (..),
  asFn,
  Type (..),
  Block (..),
  Expr (..),
  needsSemicolon,
  LetExpr (..),
  CallExpr (..),
)
where

import Named (Named (..))
import OfKind (OfKind (..))
import Source.View (HasView (..), Viewed (un))

data AST
  = AST String [Item]

newtype Item
  = FnItem Fn

asFn :: Item -> Fn
asFn (FnItem f) = f

instance OfKind Item where
  kind (FnItem f) = kind f

instance HasView Item where
  view (FnItem f) = view f

instance Named Item where
  name (FnItem f) = name f

data Fn
  = Fn {header :: Header, body :: Block}

data Block
  = Block [Expr] Expr

data Expr
  = Unit
  | Let LetExpr
  | Call CallExpr
  | Var String
  | Str String
  | Int Integer

data CallExpr
  = CallExpr String [Expr]

data LetExpr
  = LetExpr String Expr

instance OfKind Fn where
  kind _ = "function"

instance HasView Fn where
  view (Fn h _) = view h

instance Named Fn where
  name (Fn h _) = name h

data Header
  = Header
  { name_ :: Viewed String
  , retType :: Type
  }

instance HasView Header where
  view h = view (name_ h)

instance Named Header where
  name h = un (name_ h)

data Type
  = Void

needsSemicolon :: Expr -> Bool
needsSemicolon _ = True
