module Language.Parser.Error.Throw
  ( failParse,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.State (gets)
import Control.Monad.Writer (tell)
import Language.Parser (Parser)
import Language.Parser.Error (Error (..))
import Language.Parser.State.Pos (pos)
import Language.Parser.Util.View (view)

failParse :: String -> Parser a
failParse msg = do
  p <- gets pos
  v <- view p p
  tell (Error v [msg])
  throwError ()
