module Language.Trust.Compile
  ( compile,
  )
where

import Control.Arrow (left)
import Control.Monad ((>=>))
import Language.IR.Program (Program)
import Language.Trust.AST.Parse (parse)
import Language.Trust.Error (Error (..))
import Language.Trust.Program.Build (build)
import Language.Trust.Program.Check (check)

compile :: String -> String -> Either Error Program
compile n =
  left Parse
    . parse n
    >=> left Build
      . build
    >=> left Check
      . check
