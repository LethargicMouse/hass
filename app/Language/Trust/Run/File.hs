module Language.Trust.Run.File
  ( runFile,
  )
where

import Language.Trust.Run.Code (runCode)

runFile :: FilePath -> [String] -> IO ()
runFile n args = do
  putStrLn ("> running `" ++ n ++ "`:")
  readFile n >>= runCode n args
