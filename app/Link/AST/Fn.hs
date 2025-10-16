{-# LANGUAGE TemplateHaskell #-}

-- provides data type to describe Link `Fn` AST
module Link.AST.Fn (
  Fn (..),
  header,
  body,
) where

import Control.Lens (makeLenses)
import Link.AST.Block (Block)
import Link.AST.Header (Header)
import Named (Named (..))
import Source.View (HasView (..))

data Fn
  = Fn {_header :: Header, _body :: Block}

makeLenses ''Fn

instance HasView Fn where
  view = header . view

instance Named Fn where
  name = header . name
