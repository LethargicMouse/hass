{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Link.AST (
  Extern (Extern),
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

import Control.Lens (Lens', makeLenses)
import Named (Named (..))
import OfKind (OfKind (..))
import Source.View (HasView (..), Viewed (Viewed))
import Unwrap (Unwrap (..))

data AST
  = AST String [Item]

data Item
  = FnItem Fn
  | ExItem Extern

newtype Extern
  = Extern {_unwrap_ :: Header}

data Header
  = Header
  { _name' :: Viewed String
  , _retType :: Type
  }

data Type
  = Void
  | Name String
  | I32

instance Show Type where
  show Void = "()"
  show (Name n) = n
  show I32 = "i32"

data Fn
  = Fn {_header :: Header, _body :: Block}

data Block
  = Block [Expr] Expr

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

makeLenses ''Fn
makeLenses ''Header
makeLenses ''Extern

instance Unwrap Header Extern where
  unwrap = unwrap_

instance HasView Extern where
  view = (unwrap :: Lens' Extern Header) . view

asFn :: Item -> Fn
asFn (FnItem f) = f
asFn _ = undefined

instance OfKind Item where
  kind (FnItem _) = "function"
  kind (ExItem _) = "extern item"

instance HasView Item where
  view f (FnItem fn) = FnItem <$> view f fn
  view f (ExItem ex) = ExItem <$> view f ex

instance Named Item where
  name f (FnItem fn) = FnItem <$> name f fn
  name f (ExItem ex) = ExItem <$> name f ex

instance Named Extern where
  name = (unwrap :: Lens' Extern Header) . name

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
  = FieldPostfix (Viewed String)

asName :: Type -> String
asName (Name n) = n
asName _ = undefined
