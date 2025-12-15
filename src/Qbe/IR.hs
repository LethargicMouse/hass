{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Qbe.IR where

import Effect.File (File, writeFile)
import Effectful (Eff, (:>))
import Text (Render (..))
import Prelude hiding (writeFile)

data IR
  = IR

instance Render IR where
  render = const "export function w $main() {\n@start\nret 0\n}"

dump :: (File :> es) => IR -> Eff es ()
dump = writeFile "out.qbe" . render
