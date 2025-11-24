module Qbe.IR.Stmt where

import Qbe.IR.Type (AbiType, Type)
import Qbe.IR.Value (Tmp)

data Stmt
  = Ca Call
  | Co Copy
  | R Ret

instance Show Stmt where
  show (Ca call) = show call
  show (Co copy) = show copy
  show (R ret) = show ret

data Call
  = Call Tmp AbiType String [Arg]

data Arg
  = Arg AbiType Tmp

instance Show Arg where
  show (Arg t n) = show t ++ " %t" ++ show n

instance Show Call where
  show (Call tm ty n as) =
    "%t"
      ++ show tm
      ++ " ="
      ++ show ty
      ++ " call $"
      ++ show n
      ++ "("
      ++ concatMap ((++ ",") . show) as

data Copy
  = Copy Tmp Type Int

instance Show Copy where
  show (Copy tm ty n) = "%t" ++ show tm ++ " =" ++ show ty ++ " copy " ++ show n

newtype Ret
  = Ret Int

instance Show Ret where
  show (Ret n) = "ret %t" ++ show n
