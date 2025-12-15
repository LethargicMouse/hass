{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text where

import Combinators ((-$), (.:))
import Data.ByteString.Builder (Builder, charUtf8, intDec)
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.String (fromString)

type Text = Builder

text :: (Show a) => a -> Text
text = render . show

sep :: Text -> [Text] -> Text
sep = fold .: intersperse

quote :: Text -> Text -> Text
quote q s = q <> s <> q

block :: String -> String -> Text
block n s =
  "\n"
    <> line pad
    <> quote " " (fromString n)
    <> line end
    <> "\n"
    <> fromString s
    <> maybeLn
    <> line full
 where
  line = fromString . (replicate -$ '-')
  pad = 4
  end = full - pad - length n
  full = 40
  maybeLn
    | null s || last s == '\n' = ""
    | otherwise = "\n"

class Render a where
  render :: a -> Text

instance Render Int where
  render = intDec

instance Render String where
  render = fromString

instance Render Char where
  render = charUtf8

instance Render Text where
  render = id
