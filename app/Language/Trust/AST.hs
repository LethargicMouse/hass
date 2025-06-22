module Language.Trust.AST
  ( AST (..),
  )
where

import Language.Trust.AST.Top (Top)

data AST
  = AST String [Top]
