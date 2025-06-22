module Language.Trust.Program.Empty
  ( empty,
  )
where

import qualified Data.Map as M
import Language.Trust.Program (Program (..))

empty :: Program
empty =
  Program
    "<unknown>"
    M.empty
    M.empty
    M.empty
