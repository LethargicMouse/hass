module Language.Trust.Run.File
  ( runFile,
  )
where

import Language.Trust.Run.Code (runCode)

runFile :: FilePath -> IO ()
runFile n = readFile n >>= runCode n
