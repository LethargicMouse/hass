{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text where

import Combinators ((-$), (.:))
import Data.ByteString.Builder (Builder, byteString, charUtf8, intDec)
import Data.ByteString.Char8 (ByteString, length)
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.String (fromString)
import Prelude hiding (length)

type Text = Builder

text :: (Show a) => a -> Text
text = render . show

sep :: Text -> [Text] -> Text
sep = fold .: intersperse

quote :: Text -> Text -> Text
quote q s = q <> s <> q

block :: ByteString -> String -> Text
block n s =
  "\n"
    <> line pad
    <> quote " " (render n)
    <> line end
    <> "\n"
    <> render s
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

leftpad :: Int -> ByteString -> Text
leftpad n s = render (replicate (n - length s) ' ') <> render s

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

instance Render ByteString where
  render = byteString
