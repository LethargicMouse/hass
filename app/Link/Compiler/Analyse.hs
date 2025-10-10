{-# LANGUAGE FlexibleContexts #-}

module Link.Compiler.Analyse (analyse, Error) where

import Control.Lens (to, view)
import Control.Monad.Except (MonadError, modifyError, throwError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (execStateT)
import Data.Map ((!?))
import Link.Program (Program (..), items, name)
import Link.Program.Info (Info, empty)
import String.Enclosed (enclosed)

analyse :: (MonadReader Program m, MonadError Error m) => m Info
analyse = flip execStateT empty $ do
  modifyError NM checkMain

checkMain :: (MonadReader Program m, MonadError NoMain m) => m ()
checkMain = do
  mm <- view $ items . to (!? "main")
  case mm of
    Nothing -> view name >>= throwError . NoMain
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
