{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Link.Parse where

import Control.Lens (makeLenses, view)
import DList (DList, append, dList)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.State.Static.Local (State, gets, runState)
import Link.AST (AST (..), Expr)
import qualified Link.AST as A
import Link.Lex (Lexeme (..), Location, Token (..), fakeLocation, start)
import Shorts (Dies, assign, die, modifying)

data Parse
  = Parse
  { _errLocation :: Location
  , _tokens :: [Token]
  , _msgs :: DList String
  }

makeLenses ''Parse

type Parses es = (State Parse :> es, Error () :> es)

parse :: (Dies es) => Eff (Error () : State Parse : es) a -> [Token] -> Eff es a
parse p ts =
  runState (initParse ts) (runErrorNoCallStack p) >>= \case
    (Left _, p') -> die (parseError p')
    (Right a, _) -> pure a

ast :: (Parses es) => Eff es AST
ast =
  AST
    <$ tok (Name "fn")
    <* name
    <* tok ParL
    <* tok ParR
    <* tok CurL
    <*> expr
    <* tok CurR

tok :: (Parses es) => Lexeme -> Eff es ()
tok l = do
  l' <- gets (lexeme . head . view tokens)
  if l == l'
    then consume
    else failParse (show l)

name :: (Parses es) => Eff es String
name =
  gets (lexeme . head . view tokens) >>= \case
    Name n -> n <$ consume
    _ -> failParse "name"

consume :: (State Parse :> es) => Eff es ()
consume = modifying tokens (tail @Token)

expr :: (Parses es) => Eff es Expr
expr = A.Int <$> int

int :: (Parses es) => Eff es Int
int =
  gets (lexeme . head . view tokens) >>= \case
    Int i -> i <$ consume
    _ -> failParse "int"

failParse :: (Parses es) => String -> Eff es a
failParse s = do
  p <- start <$> locate
  p' <- gets (start . view errLocation)
  case compare p p' of
    LT -> pure ()
    EQ -> locateErr >> modifying msgs (append s)
    GT -> locateErr >> assign msgs (dList [s])
  throwError ()

locateErr :: (State Parse :> es) => Eff es ()
locateErr = locate >>= assign errLocation

locate :: (State Parse :> es) => Eff es Location
locate = gets (location . head . view tokens)

initParse :: [Token] -> Parse
initParse ts = Parse fakeLocation ts (dList [])

parseError :: Parse -> String
parseError p =
  "! error parsing " ++ show (view errLocation p)
