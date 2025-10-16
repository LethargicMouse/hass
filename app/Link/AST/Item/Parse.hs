-- provides `Parse` for Link `Item` AST
module Link.AST.Item.Parse (item) where

import Control.Applicative ((<|>))
import Link.AST.Extern.Parse (extern)
import Link.AST.Fn.Parse (fn)
import Link.AST.Item (Item (..))
import Source.Parse (Parse)

item :: Parse Item
item =
  FnItem <$> fn
    <|> ExItem <$> extern
