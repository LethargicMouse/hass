{-# LANGUAGE TemplateHaskell #-}

module Link.AST (
  Postfix (..),
  AST (..),
  Item (..),
  Fn (Fn),
  Header (Header),
  asFn,
  Type (..),
  Block (..),
  Expr (..),
  AtomExpr (..),
  needsSemicolon,
  LetExpr (..),
  CallExpr (..),
  FieldExpr (..),
  header,
  body,
  retType,
  addPostfix,
  asName,
)
where

import Control.Lens (makeLenses)
import Named (Named (..))
import OfKind (OfKind (..))
import Source.View (HasView (..), Viewed (Viewed), unwrap)

data AST
  = AST String [Item]

newtype Item
  = FnItem Fn

data Header
  = Header
  { _name' :: Viewed String
  , _retType :: Type
  }

data Type
  = Void
  | Name String

data Fn
  = Fn {_header :: Header, _body :: Block}

data Block
  = Block [Expr] Expr

data AtomExpr
  = Unit
  | Let LetExpr
  | Call CallExpr
  | Var String
  | Str String
  | Int Integer

data FieldExpr
  = FieldExpr (Viewed Expr) String

data Expr
  = Atomic AtomExpr
  | Field FieldExpr

data CallExpr
  = CallExpr String [Expr]

data LetExpr
  = LetExpr String Expr

makeLenses ''Fn
makeLenses ''Header

asFn :: Item -> Fn
asFn (FnItem f) = f

instance OfKind Item where
  kind (FnItem f) = kind f

instance HasView Item where
  view f (FnItem fn) = FnItem <$> view f fn

instance Named Item where
  name f (FnItem fn) = FnItem <$> name f fn
instance OfKind Fn where
  kind _ = "function"

instance HasView Fn where
  view = header . view

instance Named Fn where
  name = header . name

instance HasView Header where
  view = name' . view

instance Named Header where
  name = name' . unwrap
needsSemicolon :: Expr -> Bool
needsSemicolon _ = True

addPostfix :: Viewed Expr -> Viewed Postfix -> Viewed Expr
addPostfix e (Viewed v (FieldPostfix f)) =
  Viewed v $
    Field (FieldExpr e f)

newtype Postfix
  = FieldPostfix String

asName :: Type -> String
asName (Name n) = n
asName _ = undefined
