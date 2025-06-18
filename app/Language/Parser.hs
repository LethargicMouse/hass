module Language.Parser
  ( Parser,
    runParser,
  )
where

import Control.Arrow (left)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Writer (Writer, runWriter)
import Language.Parser.Error (Error)
import Language.Parser.State (State)
import Language.Parser.State.New (new)

type Parser a = StateT State (ExceptT () (Writer Error)) a

runParser :: Parser a -> String -> String -> Either Error a
runParser p n s =
  let (ma, e) = runWriter $ runExceptT $ evalStateT p (new n s)
   in left (const e) ma
