module Language.Trust.Checker.Error.NoField
  ( NF (..),
  )
where

import Language.Trust.Type (Type)
import Language.View (View)

data NF
  = NF View String Type

instance Show NF where
  show (NF v f t) =
    "! error "
      ++ show v
      ++ "\n--! field `"
      ++ f
      ++ "` not found for type `"
      ++ show t
      ++ "`"
