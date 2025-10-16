{-# LANGUAGE FlexibleContexts #-}

-- provides a function to build a QBE `IR` from `Source`
module Link.Compiler.Build (build) where

import Combinators ((<!>))
import Control.Monad.Except (MonadError)
import Link.AST.Parse (ast)
import Link.AST.Structure (structure)
import Link.AST.Structure.Error (AlreadyExists)
import Link.Compiler.Analyse (analyse)
import qualified Link.Compiler.Analyse as Analyse
import Link.Compiler.Generate (generate)
import Qbe.Ir (IR (..))
import Source (Source)
import Source.Parse (parse)
import qualified Source.Parse.Error as Parse

build :: (MonadError Error m) => Source -> m IR
build s = do
  a <- P <!> parse ast s
  p <- AE <!> structure a
  i <- A <!> analyse p
  pure (generate i p)

data Error
  = P Parse.Error
  | A Analyse.Error
  | AE AlreadyExists

instance Show Error where
  show (P e) = show e
  show (A e) = show e
  show (AE e) = show e
