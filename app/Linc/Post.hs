{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Linc.Post where

import Command (call, run)
import Effectful (Eff, IOE, (:>))

postcompile :: (IOE :> es) => Eff es ()
postcompile = qbe >> cc

outQbe, outS, out :: FilePath
outQbe = "out.qbe"
outS = "out.s"
out = "out"

qbe :: (IOE :> es) => Eff es ()
qbe = call "qbe" ["-o", outS, outQbe]

cc :: (IOE :> es) => Eff es ()
cc = call "cc" ["-o", out, outS]

runOut :: (IOE :> es) => Eff es ()
runOut = run ("./" ++ out) []
