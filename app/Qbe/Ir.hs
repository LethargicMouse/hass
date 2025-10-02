module Qbe.Ir
  ( IR (..),
    dump,
    empty,
    Fn (..),
    addFn,
    Type (..),
    Stmt (..),
    Value (..),
  )
where

newtype IR
  = IR [Fn]

instance Show IR where
  show (IR (f : fs)) =
    "export "
      ++ show f
      ++ concatMap (('\n' :) . show) fs
  show (IR []) = ""

dump :: IR -> IO ()
dump = writeFile "out.qbe" . show

empty :: IR
empty = IR []

data Stmt
  = Label String
  | Ret Value
  | Copy String Type Value

instance Show Stmt where
  show (Label s) = '@' : s
  show (Ret v) = "  ret " ++ show v
  show (Copy i t v) =
    "  %"
      ++ i
      ++ " ="
      ++ show t
      ++ " copy "
      ++ show v

data Value
  = Tmp String
  | Int Int

instance Show Value where
  show (Tmp i) = '%' : i
  show (Int a) = show a

data Fn
  = Fn Type String [Stmt]

instance Show Fn where
  show (Fn t n ss) =
    "function "
      ++ show t
      ++ " $"
      ++ n
      ++ "() {"
      ++ concatMap (("\n  " ++) . show) ss
      ++ "\n}"

data Type
  = Word

instance Show Type where
  show Word = "w"

addFn :: Fn -> IR -> IR
addFn f (IR fs) = IR (f : fs)
