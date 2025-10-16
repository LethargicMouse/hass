-- provides `Parse` for Link `Header` AST
module Link.AST.Header.Parse (header) where

import Link.AST.Header (Header (..))
import Link.AST.Type (Type (Void))
import Source.Parse (Parse)
import Source.Parse.Common (str, viewed)
import Source.Parse.Common.Name (name)

header :: Parse Header
header = Header <$ str "fn" <*> viewed name <* str "(" <* str ")" <*> pure Void
