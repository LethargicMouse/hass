module Language.Trust.Run.File
  ( runFile,
  )
where

import Language.Trust.Run.Code (runCode)

runFile :: FilePath -> IO ()
runFile n = do
  putStrLn ("> running `" ++ n ++ "`:")
  readFile n >>= runCode n
