module Language.Parser.Util.Sep
  ( sep,
  )
where

import Control.Applicative (many, (<|>))
import Language.Parser (Parser)
import Language.Parser.Util.Str (str)

sep :: String -> Parser a -> Parser [a]
sep c p =
  do
    r <- p
    rs <- many (str c *> p)
    pure (r : rs)
    <|> pure []
