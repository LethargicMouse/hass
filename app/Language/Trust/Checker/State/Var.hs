module Language.Trust.Checker.State.Var
  ( Var (..),
  )
where

import Language.Trust.Type (Type)

newtype Var
  = Var Type
