{-# LANGUAGE OverloadedStrings #-}

module Text where

import Data.String (IsString, fromString)

newtype Text
  = Text (String -> String)

instance Show Text where
  show (Text s) = s ""

instance Semigroup Text where
  Text a <> Text b = Text (a . b)

instance Monoid Text where
  mempty = ""

instance IsString Text where
  fromString = Text . (++)

enclosed :: Text -> Text -> Text
enclosed a s = a <> s <> a

text :: (Show a) => a -> Text
text = fromString . show

block :: Text -> String -> Text
block n t =
  line padding
    <> enclosed " " n
    <> line rest
    <> "\n"
    <> fromString t
    <> if null t || last t == '\n' then "" else "\n"
 where
  line m = fromString (replicate m '-')
  padding = 4
  rest = lineLen - padding - 2
  lineLen = 40
