module Language.Trust.Builder.Error
  ( Error (..),
  )
where

import Language.Trust.Builder.Error.AlreadyExists (AE)

newtype Error
  = AlreadyExists AE

instance Show Error where
  show (AlreadyExists e) = show e
