module Language.Trust.Type
  ( Type (..),
  )
where

data Type
  = Unit
  | Name String

-- if new n {}: t then show t == n
instance Show Type where
  show Unit = "()"
  show (Name n) = n
