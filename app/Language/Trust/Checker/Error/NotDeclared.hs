module Language.Trust.Checker.Error.NotDeclared
  ( ND (..),
  )
where

import Language.View (View)

data ND
  = ND View String String

instance Show ND where
  show (ND v k n) =
    "! error "
      ++ show v
      ++ "\n--! "
      ++ k
      ++ " `"
      ++ n
      ++ "` is not declared"
