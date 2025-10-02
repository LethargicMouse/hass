{-# LANGUAGE FlexibleContexts #-}

module Link.Compiler.Analyse (analyse, Error) where

import Control.Monad.Except (MonadError, modifyError, throwError)
import Control.Monad.Reader (MonadReader, asks)
import qualified Data.Map as M
import Link.Program (Program (..))
import Link.Program.Info (Info (Info))
import String.Enclosed (enclosed)

analyse :: (MonadReader Program m, MonadError Error m) => m Info
analyse = do
  modifyError NM checkMain
  pure Info

checkMain :: (MonadReader Program m, MonadError NoMain m) => m ()
checkMain = do
  mm <- asks (M.lookup "main" . items)
  case mm of
    Nothing -> asks name >>= throwError . NoMain
    Just _ -> pure ()

newtype Error
  = NM NoMain

instance Show Error where
  show (NM e) = show e

newtype NoMain
  = NoMain String

instance Show NoMain where
  show (NoMain n) =
    "! error in "
      ++ enclosed "`" n
      ++ ":\n--! `main` function not declared"
