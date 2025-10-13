{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Link.Compiler.Analyse (analyse, Error, success) where

import Control.Lens (makeLenses, modifying, to, (^.))
import qualified Control.Lens as L
import Control.Monad (void)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState, execStateT, gets)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Hashable (Hashable (hash))
import Data.Map (Map, elems, insert, (!?))
import qualified Data.Map as M
import Enclosed (enclosed)
import Link.AST (AtomExpr (..), Block (Block), CallExpr (..), Expr (Atomic, Field), Extern, FieldExpr (..), Fn, Header, Item (..), LetExpr (LetExpr), Type (..), body, header, retType)
import Link.Program (Program (..), TypeItem (..), fields, items, name, typeItems)
import Link.Program.Info (Info, empty, types)
import OfKind (kind)
import Source.View (Viewed, un, view)
import Unwrap (unwrap)
import Zoom (storeError, storeError_, zoom)

type Vars = Map String Type'

data Type'
  = Real Type
  | Error

data Checker
  = Checker
  { _result :: Info
  , _vars :: Vars
  }

makeLenses ''Checker

analyse :: (MonadReader Program m, MonadWriter Error m) => m Info
analyse = fmap (L.view result) . flip execStateT newChecker $ do
  storeError_ NM checkMain
  mapM_ checkItem . elems =<< L.view items

newChecker :: Checker
newChecker =
  Checker
    empty
    M.empty

checkItem ::
  ( MonadReader Program m
  , MonadState Checker m
  , MonadWriter Error m
  ) =>
  Item ->
  m ()
checkItem (FnItem f) = checkFn f
checkItem (ExItem e) = checkExtern e

checkExtern :: (Applicative m) => Extern -> m ()
checkExtern _ = pure ()

checkFn ::
  ( MonadReader Program m
  , MonadState Checker m
  , MonadWriter Error m
  ) =>
  Fn ->
  m ()
checkFn f = void $ checkBlock (f ^. body)

checkBlock ::
  ( MonadReader Program m
  , MonadState Checker m
  , MonadWriter Error m
  ) =>
  Block ->
  m Type'
checkBlock (Block ss e) = do
  mapM_ checkExpr ss
  checkExpr e

checkExpr ::
  ( Monad m
  , MonadReader Program m
  , MonadState Checker m
  , MonadWriter Error m
  ) =>
  Expr ->
  m Type'
checkExpr (Atomic e) = checkAtom e
checkExpr (Field e) = checkField e

checkAtom ::
  ( MonadReader Program m
  , MonadState Checker m
  , MonadWriter Error m
  ) =>
  AtomExpr ->
  m Type'
checkAtom Unit = pure (Real Void)
checkAtom (Let e) = checkLet e
checkAtom (Call e) = checkCall e
checkAtom (Var e) = zoom vars $ checkVar e
checkAtom (Str e) = checkStr e
checkAtom (Int e) = checkInt e

checkInt :: (Applicative m) => Integer -> m Type'
checkInt _ = pure (Real I32)

checkStr :: (Applicative m) => String -> m Type'
checkStr _ = pure (Real $ Name "str")

checkVar ::
  ( MonadState Vars m
  , MonadWriter Error m
  ) =>
  Viewed String ->
  m Type'
checkVar n = do
  mt <- gets (!? un n)
  case mt of
    Just t -> pure t
    Nothing ->
      Error <$ tell (ND $ NotDeclared n "variable")

data NotDeclared
  = NotDeclared (Viewed String) String

instance Show NotDeclared where
  show (NotDeclared s k) =
    "! error "
      ++ (s ^. view . to show)
      ++ "\n--! "
      ++ k
      ++ enclosed "`" (un s)
      ++ " is not declared"

checkCall ::
  ( MonadReader Program m
  , MonadState Checker m
  , MonadWriter Error m
  ) =>
  CallExpr ->
  m Type'
checkCall (CallExpr n es) = do
  mapM_ checkExpr es
  mh <- storeError id (findHeader n)
  case mh of
    Nothing -> pure Error
    Just h -> pure (Real $ h ^. retType)

findHeader ::
  ( MonadReader Program m
  , MonadError Error m
  ) =>
  Viewed String ->
  m Header
findHeader n = do
  mi <- L.view $ items . to (!? un n)
  case mi of
    Just (FnItem f) -> pure $ f ^. header
    Just i ->
      throwError . WK $
        WrongKind n (kind i) "function"
    Nothing ->
      throwError . ND $
        NotDeclared n "function"

checkLet ::
  ( MonadReader Program m
  , MonadState Checker m
  , MonadWriter Error m
  ) =>
  LetExpr ->
  m Type'
checkLet (LetExpr n e) = do
  t <- checkExpr e
  modifying vars (insert n t)
  pure (Real Void)

checkField ::
  ( MonadReader Program m
  , MonadWriter Error m
  , MonadState Checker m
  ) =>
  FieldExpr ->
  m Type'
checkField (FieldExpr e f) = do
  t <- checkExpr (e ^. unwrap)
  mt' <- makeReal t
  case mt' of
    Nothing -> pure ()
    Just t' ->
      modifying (result . types) $
        insert (e ^. view . to hash) t'
  getField t f

makeReal :: (Applicative m) => Type' -> m (Maybe Type)
makeReal (Real t) = pure (Just t)
makeReal Error = pure Nothing

getField ::
  ( MonadReader Program m
  , MonadWriter Error m
  ) =>
  Type' ->
  Viewed String ->
  m Type'
getField Error _ = pure Error
getField (Real (Name n)) f = do
  mi <- L.view (typeItems . to (!? n))
  case mi of
    Just (StructItem s) -> case s ^. fields . to (!? un f) of
      Nothing -> Error <$ tell (NF $ NoField (Name n) f)
      Just t -> pure (Real t)
    _ -> pure Error
getField (Real t) f = Error <$ tell (NF $ NoField t f)

data NoField
  = NoField Type (Viewed String)

instance Show NoField where
  show (NoField t s) =
    "! error "
      ++ show (s ^. view)
      ++ "\n--! type "
      ++ enclosed "`" (show t)
      ++ " is not declared"

checkMain :: (MonadReader Program m, MonadError NoMain m) => m ()
checkMain = do
  mm <- L.view $ items . to (!? "main")
  case mm of
    Nothing -> L.view name >>= throwError . NoMain
    Just _ -> pure ()

data WrongKind
  = WrongKind (Viewed String) String String

instance Show WrongKind where
  show (WrongKind n f e) =
    "! error "
      ++ show (n ^. view)
      ++ "\n--! "
      ++ enclosed "`" (un n)
      ++ " is a "
      ++ f
      ++ ", but "
      ++ e
      ++ " was expected"

data Error
  = NM NoMain
  | WK WrongKind
  | ND NotDeclared
  | NF NoField
  | Es Error Error
  | Ner

instance Semigroup Error where
  a <> Ner = a
  Ner <> b = b
  a <> b = Es a b

instance Monoid Error where
  mempty = Ner

instance Show Error where
  show (NM e) = show e
  show (WK e) = show e
  show (ND e) = show e
  show (NF e) = show e
  show (Es e1 e2) = show e1 ++ "\n\n" ++ show e2
  show Ner = ""

newtype NoMain
  = NoMain String

instance Show NoMain where
  show (NoMain n) =
    "! error in "
      ++ enclosed "`" n
      ++ ":\n--! `main` function not declared"

success :: Error -> Bool
success Ner = True
success _ = False
