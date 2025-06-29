module Language.Trust.AST.Parse
  ( parse,
  )
where

import Language.Parser.Error (Error)
import Language.Parser.Run (runParser)
import Language.Trust.AST (AST)
import Language.Trust.AST.Parser (ast)

parse :: String -> String -> Either Error AST
parse = runParser ast
