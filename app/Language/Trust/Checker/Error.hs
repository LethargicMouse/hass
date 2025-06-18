module Language.Trust.Checker.Error
  ( Error (..),
  )
where

newtype Error
  = NoMain String

instance Show Error where
  show (NoMain s) =
    "! error in `" ++ s ++ "`\n--! `main` function is not declared"
