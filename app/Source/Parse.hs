{-# LANGUAGE FlexibleContexts #-}

module Source.Parse (
  Parse,
  parse,
  failParse,
) where

import Control.Monad.Except (ExceptT, MonadError, liftEither, modifyError, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT, gets)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Set (singleton)
import Source (Info, Source (..))
import Source.Code (Code)
import Source.Code.Getters (nextPos)
import Source.Info.Getters (viewAt)
import Source.Parse.Error (Error (..))

type Parse a = ReaderT Info (StateT Code (ExceptT () (Writer Error))) a

parse :: (MonadError Error m) => Parse a -> Source -> m a
parse p (Source i c) =
  let (a, b) =
        runWriter $
          runExceptT $
            p
              `runReaderT` i
              `evalStateT` c
   in modifyError (const b) (liftEither a)

failParse :: String -> Parse a
failParse msg = do
  p <- gets nextPos
  tell . Error (singleton msg) =<< viewAt p
  throwError ()
