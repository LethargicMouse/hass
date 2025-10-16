-- provides data type to describe Link `Type` AST
module Link.AST.Type (
  Type (..),
  asName,
) where

data Type
  = Void
  | Name String
  | I32

instance Show Type where
  show Void = "()"
  show (Name n) = n
  show I32 = "i32"

asName :: Type -> String
asName (Name n) = n
asName _ = undefined
