{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Qbe.IR where

import Data.ByteString.Builder (Builder)
import Effect.File (File, writeFile)
import Effectful (Eff, (:>))
import Prelude hiding (writeFile)

data IR
  = IR

render :: IR -> Builder
render _ = "export function w $main() {\n@start\nret 0\n}"

dump :: (File :> es) => IR -> Eff es ()
dump = writeFile "out.qbe" . render
