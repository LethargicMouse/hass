module BaseFix (head, readFile) where

import BaseFix.ReadFile (readFile)
import Prelude hiding (head, readFile)

head :: [a] -> a
head [] = undefined
head (a : _) = a
