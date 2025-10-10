{-# LANGUAGE FlexibleContexts #-}

module Link.Compiler.Parse (parse, ast) where

import BaseFix (head)
import Control.Applicative (many, some, (<|>))
import Control.Lens (view)
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, MonadError, liftEither, modifyError, runExceptT, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT, gets, modify)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (foldl', isPrefixOf)
import Data.Set (singleton)
import Link.AST (AST (AST), AtomExpr (..), Block (..), CallExpr (CallExpr), Expr (..), Fn (Fn), Header (..), Item (..), LetExpr (..), Postfix (FieldPostfix), Type (..), addPostfix, needsSemicolon)
import Link.Compiler.Parse.Error (Error (Error))
import Source (Code (..), Info (srcName), Source (Source), codeLines, nextChar, pos, text)
import Source.Pos (Pos, nextPos)
import Source.View (View (View), Viewed (..), unwrap)
import String.Enclosed (enclosed)
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
name = name' <|> failParse "name"

name' :: Parse String
name' = do
  skipSpaces
  t <- gets (takeWhile isNameChar . text)
  if not (null t) && isName1Char (head t)
    then t <$ consume (length t)
    else throwError ()

skipSpaces :: Parse ()
skipSpaces = modify (Code . dropWhile (isSpace . snd) . unCode)

isName1Char :: Char -> Bool
isName1Char c = isAlpha c || c == '_'

isNameChar :: Char -> Bool
isNameChar c = isName1Char c || isDigit c

str :: String -> Parse ()
str s = str' s <|> failParse (enclosed "`" s)

str' :: String -> Parse ()
str' s = do
  skipSpaces
  t <- gets text
  if s `isPrefixOf` t
    then consume (length s)
    else throwError ()

consume :: (MonadState Code m) => Int -> m ()
consume n = modify (Code . drop n . unCode)

block :: Parse Block
block =
  Block
    <$ str "{"
    <*> many stmt
    <*> (view unwrap <$> expr <|> pure (Atomic Unit))
    <* str "}"

stmt :: Parse Expr
stmt = do
  e <- view unwrap <$> expr
  e <$ when (needsSemicolon e) (str ";")

expr :: Parse (Viewed Expr)
expr = foldl' addPostfix . fmap Atomic <$> viewed atom <*> many (viewed postfix)

postfix :: Parse Postfix
postfix = FieldPostfix <$ str "." <*> name

atom :: Parse AtomExpr
atom =
  Unit <$ str' "()"
    <|> Let <$> letExpr
    <|> Call <$> callExpr
    <|> Var <$> varExpr
    <|> Str <$> strLiteral
    <|> Int <$> intLiteral
    <|> failParse "expr"

intLiteral :: Parse Integer
intLiteral = read <$> some (satisfy isDigit)

callExpr :: Parse CallExpr
callExpr =
  CallExpr
    <$> name'
    <* str "("
    <*> manySep "," (view unwrap <$> expr)
    <* str ")"

manySep :: String -> Parse a -> Parse [a]
manySep c p = someSep c p <|> pure []

someSep :: String -> Parse a -> Parse [a]
someSep c p = (:) <$> p <*> many (str c *> p)

strLiteral :: Parse String
strLiteral =
  read . enclosed "\""
    <$ str' "\""
    <*> many strChar
    <* str "\""

strChar :: Parse Char
strChar = satisfy (not . flip elem ['\\', '"'])

satisfy :: (Char -> Bool) -> Parse Char
satisfy p = do
  c <- gets nextChar
  if p c
    then c <$ consume 1
    else throwError ()

varExpr :: Parse String
varExpr = name'

letExpr :: Parse LetExpr
letExpr =
  LetExpr
    <$ str' "let"
    <*> name
    <* str "="
    <*> (view unwrap <$> expr)

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
  tell . Error (singleton msg) =<< at p
  throwError ()

at :: (MonadReader Info m) => Pos -> m View
at p = between p (nextPos p ' ')

between :: (MonadReader Info m) => Pos -> Pos -> m View
between s e = do
  n <- asks srcName
  ls <- asks codeLines
  pure $ View n s e ls
