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
import Language.Source.Pos (Pos (..))

failParse :: String -> Parser a
failParse msg = do
  Pos l s <- gets pos
  v <- view (Pos l s) (Pos l $ s + 1)
  tell (Error v [msg])
  throwError ()
