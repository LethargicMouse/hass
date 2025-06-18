module Language.Parser.Util.SkipSpace
  ( skipSpace,
  )
where

import Control.Lens (modifying)
import Data.Char (isSpace)
import Language.Parser (Parser)
import Language.Parser.State (rest)

skipSpace :: Parser ()
skipSpace =
  modifying rest $
    dropWhile (isSpace . fst)
