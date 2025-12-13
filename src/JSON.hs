{-# LANGUAGE OverloadedStrings #-}

module JSON where

import Data.ByteString.Builder (Builder, intDec)
import Text (sep, text)

data JSO
  = Int Int
  | String String
  | Char Char
  | Bool Bool
  | Null
  | List [JSO]
  | Object [(String, JSO)]

render :: JSO -> Builder
render (Int i) = intDec i
render (String s) = text s
render (Char c) = text c
render (Bool b) = text b
render Null = "null"
render (List as) = "[" <> sep "," (render <$> as) <> "]"
render (Object as) = "{" <> sep "," (pair <$> as) <> "}"
 where
  pair (s, o) = text s <> ": " <> render o
