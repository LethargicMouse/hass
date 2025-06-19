module Language.Parser.Run
  ( runParser,
  )
where

import Control.Arrow (left)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalStateT)
import Control.Monad.Writer (runWriter)
import Language.Parser (Parser)
import Language.Parser.Error (Error)
import Language.Parser.State.New (new)

runParser :: Parser a -> String -> String -> Either Error a
runParser p n s =
  let (ma, e) = runWriter $ runExceptT $ evalStateT p (new n s)
   in left (const e) ma
