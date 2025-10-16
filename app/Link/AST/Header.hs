{-# LANGUAGE TemplateHaskell #-}

-- provides data type to describe Link `Header` AST
module Link.AST.Header (
  Header (..),
  retType,
) where

import Control.Lens (makeLenses)
import Link.AST.Type (Type)
import Named (Named (..))
import Source.View (HasView (..), Viewed)
import Unwrap (unwrap)

data Header
  = Header
  { _name' :: Viewed String
  , _retType :: Type
  }

makeLenses ''Header

instance HasView Header where
  view = name' . view

instance Named Header where
  name = name' . unwrap
