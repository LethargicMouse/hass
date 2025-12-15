{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Effect.Stdio where

import Combinators ((-$))
import Data.ByteString.Builder (Builder, charUtf8, hPutBuilder)
import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep, getStaticRep, unsafeEff_)
import System.IO (Handle, stdout)

data Stdio :: Effect

type instance DispatchOf Stdio = Static WithSideEffects

newtype instance StaticRep Stdio = Stdio {hPutHandler :: Handle -> Builder -> IO ()}

putStrLn :: (Stdio :> es) => Builder -> Eff es ()
putStrLn b = do
  unsafeEff_ . (hPutHandler -$ stdout -$ b <> charUtf8 '\n') =<< getStaticRep

runStdio :: (IOE :> es) => Eff (Stdio : es) a -> Eff es a
runStdio = evalStaticRep (Stdio hPutBuilder)
