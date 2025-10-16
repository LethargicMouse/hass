-- provides `Parse` for Link `Fn` AST
module Link.AST.Fn.Parse (fn) where

import Link.AST.Block.Parse (block)
import Link.AST.Fn (Fn (..))
import Link.AST.Header.Parse (header)
import Source.Parse (Parse)

fn :: Parse Fn
fn = Fn <$> header <*> block
