-- provides `Parse` for Link `Block` AST
module Link.AST.Block.Parse (block) where

import Control.Applicative (many, (<|>))
import Control.Lens (view)
import Control.Monad (when)
import Link.AST.Block (Block (..))
import Link.AST.Expr (AtomExpr (Unit), Expr (Atomic), needsSemicolon)
import Link.AST.Expr.Parse (expr)
import Source.Parse (Parse)
import Source.Parse.Common (str)
import Unwrap (unwrap)

block :: Parse Block
block =
  Block
    <$ str "{"
    <*> many stmt
    <*> (view unwrap <$> expr <|> pure (Atomic Unit))
    <* str "}"

stmt :: Parse Expr
stmt = do
  e <- view unwrap <$> expr
  e <$ when (needsSemicolon e) (str ";")
