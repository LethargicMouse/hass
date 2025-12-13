module Text where

import Data.ByteString.Builder (Builder)
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.String (fromString)

type Text = Builder

text :: (Show a) => a -> Text
text = fromString . show

sep :: Builder -> [Builder] -> Builder
sep c = fold . intersperse c
