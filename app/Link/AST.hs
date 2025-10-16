-- provides data type to describe Link `AST`
module Link.AST (AST (..)) where

import Link.AST.Item (Item)

data AST
  = AST String [Item]
