{-# LANGUAGE OverloadedStrings #-}

module Render where

import Data.ByteString.Builder (Builder, charUtf8)
import Data.String (fromString)

quote :: Builder -> Builder -> Builder
quote q s = q <> s <> q

block :: String -> String -> Builder
block n s =
  newline
    <> line pad
    <> quote " " (fromString n)
    <> line end
    <> newline
    <> fromString s
    <> maybeLn
    <> line full
 where
  line m = fromString (replicate m '-')
  pad = 4
  end = full - pad - length n
  full = 40
  maybeLn
    | null s || last s == '\n' = mempty
    | otherwise = newline

newline :: Builder
newline = charUtf8 '\n'
