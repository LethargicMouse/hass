module Language.Parser.Util.Str
  ( str,
    str',
  )
where

import Control.Lens (modifying, use)
import Data.List (isPrefixOf)
import Language.Parser (Parser)
import Language.Parser.Error.Throw (failParse)
import Language.Parser.State (rest)
import Language.Parser.Util.SkipSpace (skipSpace)

str :: String -> Parser ()
str s = skipSpace >> str' s

str' :: String -> Parser ()
str' s = do
  r <- map fst <$> use rest
  if s `isPrefixOf` r
    then modifying rest (drop $ length s)
    else failParse ("`" ++ s ++ "`")
