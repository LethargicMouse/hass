module Language.Trust.Program.Builder
  ( programBuilder,
  )
where

import Control.Lens (assign)
import Control.Monad (forM_)
import Language.Trust.AST (AST (..))
import Language.Trust.Builder (Builder)
import Language.Trust.Program (Program, name)
import Language.Trust.Program.Builder.Add (add)

programBuilder :: AST -> Builder Program
programBuilder (AST n ts) = do
  assign name n
  forM_ ts add
