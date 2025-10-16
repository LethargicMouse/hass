{-# LANGUAGE FlexibleContexts #-}

-- provides `Parse` for Link `AST`
module Link.AST.Parse (ast) where

import Control.Applicative (many, some, (<|>))
import Control.Lens (view)
import Control.Monad.Reader (asks)
import Data.Char (isDigit)
import Enclosed (enclosed)
import Link.AST (AST (..))
import Link.AST.Expr (AtomExpr (..), CallExpr (..), LetExpr (..))
import Link.AST.Expr.Postfix (Postfix (..))
import Link.AST.Item.Parse (item)
import Source (srcName)
import Source.Parse (Parse, failParse)
import Source.Parse.Common (eof, manySep, satisfy, str, str', viewed)
import Source.Parse.Common.Name (name, name')
import Source.View (Viewed (..))
import Unwrap (unwrap)
import Prelude hiding (head)

ast :: Parse AST
ast = asks (AST . srcName) <*> many item <* eof
