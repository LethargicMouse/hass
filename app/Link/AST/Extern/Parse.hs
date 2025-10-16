-- provides `Parse` for Link `Extern` AST
module Link.AST.Extern.Parse (extern) where

import Link.AST.Extern (Extern (..))
import Link.AST.Header.Parse (header)
import Source.Parse (Parse)
import Source.Parse.Common (str)

extern :: Parse Extern
extern =
  Extern
    <$ str "extern"
    <*> header
