module Language.Trust.Checker.Error.UnknownCommand
  ( UC (..),
  )
where

import Language.View (View)

data UC
  = UC View String

instance Show UC where
  show (UC v n) =
    "! error " ++ show v ++ "\n--! unknown command: `" ++ n ++ "`"
