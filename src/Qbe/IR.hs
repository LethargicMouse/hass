module Qbe.IR where

import Qbe.IR.Stmt (Stmt)

newtype IR
  = IR [Stmt]

instance Show IR where
  show (IR stmts) =
    "export function w $main(){\n@start"
      ++ concatMap (('\n' :) . show) stmts
