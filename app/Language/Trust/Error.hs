module Language.Trust.Error
  ( Error (..),
  )
where

import qualified Language.Parser.Error as P
import qualified Language.Trust.Builder.Error as B
import qualified Language.Trust.Checker.Error as C

data Error
  = Parse P.Error
  | Build B.Error
  | Check C.Error

instance Show Error where
  show (Parse e) = show e
  show (Build e) = show e
  show (Check e) = show e
