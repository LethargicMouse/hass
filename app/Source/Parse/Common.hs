-- Provides commonly used `Source` `Parse`s
module Source.Parse.Common where

import Control.Applicative (many, (<|>))
import Control.Lens (modifying)
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.State (gets)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Enclosed (enclosed)
import Source.Code (consume)
import Source.Code.Getters (nextChar, nextPos, text)
import Source.Info.Getters (viewBetween)
import Source.Parse (Parse, failParse)
import Source.View (Viewed (Viewed))
import Unwrap (unwrap)

viewed :: Parse a -> Parse (Viewed a)
viewed p = do
  skipSpaces
  s <- gets nextPos
  a <- p
  e <- gets nextPos
  v <- viewBetween s e
  pure (Viewed v a)

skipSpaces :: Parse ()
skipSpaces = modifying unwrap (dropWhile $ isSpace . snd)

str :: String -> Parse ()
str s = str' s <|> failParse (enclosed "`" s)

str' :: String -> Parse ()
str' s = do
  skipSpaces
  t <- gets text
  if s `isPrefixOf` t
    then consume (length s)
    else throwError ()

manySep :: String -> Parse a -> Parse [a]
manySep c p = someSep c p <|> pure []

someSep :: String -> Parse a -> Parse [a]
someSep c p = (:) <$> p <*> many (str c *> p)

satisfy :: (Char -> Bool) -> Parse Char
satisfy p = do
  c <- gets nextChar
  if p c
    then c <$ consume 1
    else throwError ()

eof :: Parse ()
eof = do
  skipSpaces
  t <- gets text
  unless
    (t == "\0")
    (failParse "end of file")
