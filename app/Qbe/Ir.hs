module Qbe.Ir (
  IR (..),
  dump,
  empty,
  Fn (..),
  addFn,
  Type (..),
  Stmt (..),
  Value (..),
  ExType (..),
  StoreStmt (..),
  CopyStmt (..),
  BinOp (..),
  AllocStmt (..),
  BinStmt (..),
  CallStmt (..),
  AbiType (..),
  abiWord,
  basicAbi,
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

data ExType
  = Basic Type
  | Half
  | Byte

instance Show ExType where
  show (Basic t) = show t
  show Half = "h"
  show Byte = "b"

data BinOp
  = Add

instance Show BinOp where
  show Add = "add"

data CopyStmt
  = CopyStmt String Type Value

instance Show CopyStmt where
  show (CopyStmt s t v) =
    "  %" ++ s ++ " =" ++ show t ++ " copy " ++ show v

data StoreStmt
  = StoreStmt ExType Value Value

instance Show StoreStmt where
  show (StoreStmt t v p) =
    "  store"
      ++ show t
      ++ " "
      ++ show v
      ++ ", "
      ++ show p

data AllocStmt
  = AllocStmt String Int Int

instance Show AllocStmt where
  show (AllocStmt i a s) = "  %" ++ i ++ " =l alloc" ++ show a ++ " " ++ show s

data BinStmt
  = BinStmt String Type Value BinOp Value

instance Show BinStmt where
  show (BinStmt i t a o b) = "  %" ++ i ++ " =" ++ show t ++ " " ++ show o ++ " " ++ show a ++ ", " ++ show b

data AbiType
  = SignedByte
  | Extra ExType
  | SignedHalf
  | Name String

instance Show AbiType where
  show SignedByte = "sb"
  show SignedHalf = "sh"
  show (Extra (Basic t)) = show t
  show (Extra t) = 'u' : show t
  show (Name n) = ':' : n

data CallStmt
  = CallStmt String AbiType Value [(AbiType, Value)]

instance Show CallStmt where
  show (CallStmt i t v as) =
    "  %"
      ++ i
      ++ " ="
      ++ show t
      ++ " call "
      ++ show v
      ++ "("
      ++ concatMap (\(at, a) -> show at ++ " " ++ show a ++ ", ") as
      ++ ")"

data Stmt
  = Label String
  | Ret Value
  | Copy CopyStmt
  | Store StoreStmt
  | Alloc AllocStmt
  | Bin BinStmt
  | Call CallStmt

instance Show Stmt where
  show (Label s) = '@' : s
  show (Ret v) = "  ret " ++ show v
  show (Store s) = show s
  show (Copy c) = show c
  show (Alloc s) = show s
  show (Bin s) = show s
  show (Call s) = show s

data Value
  = Tmp String
  | Int Int
  | Const String

instance Show Value where
  show (Tmp i) = '%' : i
  show (Const i) = '$' : i
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
  | Long
  | Float
  | Double

instance Show Type where
  show Word = "w"
  show Long = "l"
  show Float = "s"
  show Double = "d"

addFn :: Fn -> IR -> IR
addFn f (IR fs) = IR (f : fs)

abiWord :: AbiType
abiWord = Extra (Basic Word)

basicAbi :: AbiType -> Type
basicAbi = basicExtra . extraAbi

extraAbi :: AbiType -> ExType
extraAbi (Extra t) = t
extraAbi _ = undefined

basicExtra :: ExType -> Type
basicExtra (Basic t) = t
basicExtra _ = undefined
