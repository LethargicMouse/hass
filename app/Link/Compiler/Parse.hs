{-# LANGUAGE FlexibleContexts #-}

module Link.Compiler.Parse (parse, ast) where

import BaseFix (head)
import Control.Applicative (many, (<|>))
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, MonadError, liftEither, modifyError, runExceptT, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT, gets, modify)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (isPrefixOf)
import Link.AST (AST (AST), Block (..), Expr (..), Fn (Fn), Header (..), Item (..), Type (..), needsSemicolon)
import Link.Compiler.Parse.Error (Error (Error))
import Source (Code (..), Info (srcName), Source (Source), codeLines, pos, text)
import Source.Pos (Pos, nextPos)
import Source.View (View (View), Viewed (..))
import Prelude hiding (head)

parse :: (MonadError Error m) => Parse a -> Source -> m a
parse p (Source i c) =
  let (a, b) =
        runWriter $
          runExceptT $
            p
              `runReaderT` i
              `evalStateT` c
   in modifyError (const b) (liftEither a)

type Parse a = ReaderT Info (StateT Code (ExceptT () (Writer Error))) a

ast :: Parse AST
ast = asks (AST . srcName) <*> many item <* eof

item :: Parse Item
item = FnItem <$> fn

fn :: Parse Fn
fn = Fn <$> header <*> block

header :: Parse Header
header = Header <$ str "fn" <*> viewed name <* str "(" <* str ")" <*> pure Void

viewed :: Parse a -> Parse (Viewed a)
viewed p = do
  s <- gets pos
  a <- p
  e <- gets pos
  v <- between s e
  pure (Viewed v a)

name :: Parse String
name = do
  skipSpaces
  t <- gets (takeWhile isNameChar . text)
  if not (null t) && isName1Char (head t)
    then t <$ consume (length t)
    else failParse "name"

skipSpaces :: Parse ()
skipSpaces = modify (Code . dropWhile (isSpace . snd) . unCode)

isName1Char :: Char -> Bool
isName1Char c = isAlpha c || c == '_'

isNameChar :: Char -> Bool
isNameChar c = isName1Char c || isDigit c

str :: String -> Parse ()
str s = do
  skipSpaces
  t <- gets text
  if s `isPrefixOf` t
    then consume (length s)
    else failParse s

consume :: (MonadState Code m) => Int -> m ()
consume n = modify (Code . drop n . unCode)

block :: Parse Block
block =
  Block
    <$ str "{"
    <*> many stmt
    <*> (expr <|> pure Unit)
    <* str "}"

stmt :: Parse Expr
stmt = do
  e <- expr
  e <$ when (needsSemicolon e) (str ";")

expr :: Parse Expr
expr = Unit <$ str "()"

eof :: Parse ()
eof = do
  skipSpaces
  t <- gets text
  unless
    (t == "\0")
    (failParse "end of file")

failParse :: String -> Parse a
failParse msg = do
  p <- gets pos
  tell . Error [msg] =<< at p
  throwError ()

at :: (MonadReader Info m) => Pos -> m View
at p = between p (nextPos p ' ')

between :: (MonadReader Info m) => Pos -> Pos -> m View
between s e = do
  n <- asks srcName
  ls <- asks codeLines
  pure $ View n s e ls
