module Qbe.IR.Type where

newtype AbiType
  = T Type

instance Show AbiType where
  show (T t) = show t

data Type
  = Word
  | Long

instance Show Type where
  show Word = "w"
  show Long = "l"
