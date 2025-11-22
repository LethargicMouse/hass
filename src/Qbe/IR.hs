module Qbe.IR where

newtype IR
  = IR [Stmt]

instance Show IR where
  show (IR stmts) =
    "export function w $main(){\n@start"
      ++ concatMap (('\n' :) . show) stmts

data Stmt
  = Ca Call
  | Co Copy
  | R Ret

instance Show Stmt where
  show (Ca call) = show call
  show (Co copy) = show copy
  show (R ret) = show ret

type Tmp = Int

newtype AbiType
  = T Type

instance Show AbiType where
  show (T t) = show t

data Arg
  = Arg AbiType Int

instance Show Arg where
  show (Arg t n) = show t ++ " %t" ++ show n

data Call
  = Call Tmp AbiType String [Arg]

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

data Type
  = Word
  | Long

instance Show Type where
  show Word = "w"
  show Long = "l"

data Copy
  = Copy Tmp Type Int

instance Show Copy where
  show (Copy tm ty n) = "%t" ++ show tm ++ " =" ++ show ty ++ " copy " ++ show n

newtype Ret
  = Ret Int

instance Show Ret where
  show (Ret n) = "ret %t" ++ show n
