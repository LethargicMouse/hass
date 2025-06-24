module Language.Trust.Checker.Error
  ( Error (..),
  )
where

import Language.Trust.Checker.Error.NoField (NF)
import Language.Trust.Checker.Error.NotDeclared (ND)
import Language.Trust.Checker.Error.UnknownCommand (UC)

data Error
  = NoMain String
  | NotDeclared ND
  | NoField NF
  | UnknownCommand UC

instance Show Error where
  show (NoMain s) =
    "! error in `" ++ s ++ "`\n--! `main` function is not declared"
  show (NotDeclared nd) = show nd
  show (NoField nf) = show nf
