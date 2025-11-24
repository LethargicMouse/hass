{-# LANGUAGE FlexibleContexts #-}

module Linc.Args where

import Effectful (Eff)
import Shorts (Dies, die, enclosed)

getPath :: (Dies es) => [String] -> Eff es FilePath
getPath [] = die expectedPath
getPath [path] = pure path
getPath (_ : a : _) = die (unexpectedArgument a)

expectedPath :: String
expectedPath = argsError "expected argument"

argsError :: String -> String
argsError = (++) "! error reading args: "

unexpectedArgument :: String -> String
unexpectedArgument a = argsError ("unexpected argument: " ++ enclosed "`" a)
