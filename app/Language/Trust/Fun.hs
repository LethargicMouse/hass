{-# LANGUAGE TemplateHaskell #-}

module Language.Trust.Fun
  ( Fun (..),
    header,
    body,
  )
where

import Control.Lens (makeLenses)
import Language.Trust.Expr (Block)
import Language.Trust.Fun.Header (Header)

data Fun
  = Fun
  { _header :: Header,
    _body :: Block
  }

makeLenses ''Fun
