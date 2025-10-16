{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- provides data type to describe Link `Extern` AST
module Link.AST.Extern (Extern (..)) where

import Control.Lens (Lens', makeLenses)
import Link.AST.Header (Header)
import Named (Named (..))
import Source.View (HasView (..))
import Unwrap (Unwrap (unwrap))

newtype Extern
  = Extern {_unwrap_ :: Header}

makeLenses ''Extern

instance Unwrap Header Extern where
  unwrap = unwrap_

instance HasView Extern where
  view = (unwrap :: Lens' Extern Header) . view

instance Named Extern where
  name = (unwrap :: Lens' Extern Header) . name
