module Language.Trust.Program.Build
  ( build,
  )
where

import Language.Trust.AST (AST)
import Language.Trust.Builder.Error (Error)
import Language.Trust.Builder.Run (runBuilder)
import Language.Trust.Program (Program)
import Language.Trust.Program.Builder (programBuilder)
import Language.Trust.Program.Empty (empty)

build :: AST -> Either Error Program
build = flip runBuilder empty . programBuilder
