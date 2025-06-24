module Language.Parser.Util.Satisfy
  ( satisfy,
  )
where

import Control.Lens (modifying, use)
import Language.Parser (Parser)
import Language.Parser.Error.Throw (failParse)
import Language.Parser.State (rest)

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy msg p = do
  r <- map fst <$> use rest
  case r of
    c : _ | p c -> c <$ modifying rest (drop 1)
    _ -> failParse msg
