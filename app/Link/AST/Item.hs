-- provides data type to describe Link `Item` AST
module Link.AST.Item (
  Item (..),
  asFn,
) where

import Link.AST.Extern (Extern)
import Link.AST.Fn (Fn)
import Named (Named (..))
import OfKind (OfKind (..))
import Source.View (HasView (..))

data Item
  = FnItem Fn
  | ExItem Extern

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
