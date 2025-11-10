module Source where

data Source
  = Source

readSource :: FilePath -> IO Source
readSource p = source p <$> readFile p

source :: String -> String -> Source
source _ _ = Source
