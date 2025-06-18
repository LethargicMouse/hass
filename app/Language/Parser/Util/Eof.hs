module Language.Parser.Util.Eof
  ( eof,
    eof',
  )
where

import Control.Lens (use)
import Control.Monad (unless)
import Language.Parser (Parser)
import Language.Parser.Error.Throw (failParse)
import Language.Parser.State (rest)
import Language.Parser.Util.SkipSpace (skipSpace)

eof :: Parser ()
eof = skipSpace >> eof'

eof' :: Parser ()
eof' = do
  r <- use rest
  unless (null r) (failParse "end of file")
