module Language.Trust.AST
  ( AST (..),
  )
where

import Language.Trust.Fun (Fun)

data AST
  = AST String Fun
