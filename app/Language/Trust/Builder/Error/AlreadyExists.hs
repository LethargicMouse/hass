module Language.Trust.Builder.Error.AlreadyExists
  ( AE (..),
  )
where

import Language.View (View)

data AE
  = AE View String String View

instance Show AE where
  show (AE v k n v') =
    "! error "
      ++ show v
      ++ "\n--! "
      ++ k
      ++ " `"
      ++ n
      ++ "` is already declared "
      ++ show v'
