module Source where

import Effectful (Eff)

data Source = Source

readSource :: FilePath -> Eff es Source
readSource _ = pure Source
