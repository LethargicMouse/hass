{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Effect.File where

import Combinators ((-$))
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects (WithSideEffects), StaticRep, evalStaticRep, getStaticRep, unsafeEff_)
import Prelude hiding (readFile)

data File :: Effect

type instance DispatchOf File = Static WithSideEffects

data instance StaticRep File = File
  { writeFileHandler :: FilePath -> Builder -> IO ()
  , readFileHandler :: FilePath -> IO ByteString
  }

writeFile :: (File :> es) => FilePath -> Builder -> Eff es ()
writeFile p b = unsafeEff_ . (writeFileHandler -$ p -$ b) =<< getStaticRep

readFile :: (File :> es) => FilePath -> Eff es ByteString
readFile p = unsafeEff_ . (readFileHandler -$ p) =<< getStaticRep

runFile :: (IOE :> es) => Eff (File : es) a -> Eff es a
runFile =
  evalStaticRep
    File
      { writeFileHandler = B.writeFile
      , readFileHandler = BS.readFile
      }
