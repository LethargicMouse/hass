{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Effect.Exit where

import Combinators ((!.), (-$), (.>))
import Data.ByteString.Builder (Builder)
import Effect.Stdio (Stdio, putStrLn)
import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects (WithSideEffects), StaticRep, evalStaticRep, getStaticRep, unsafeEff_)
import Effectful.Error.Static (Error)
import System.Exit (ExitCode)
import qualified System.Exit as SIO
import Prelude hiding (putStrLn)

data Exit :: Effect

type instance DispatchOf Exit = Static WithSideEffects

data instance StaticRep Exit = Exit
  { exitWithHandler :: forall a. ExitCode -> IO a
  , exitFailureHandler :: forall a. IO a
  }

exitWith :: (Exit :> es) => ExitCode -> Eff es a
exitWith c = unsafeEff_ . (exitWithHandler -$ c) =<< getStaticRep

exitFailure :: (Exit :> es) => Eff es a
exitFailure = unsafeEff_ . exitFailureHandler =<< getStaticRep

runExit :: (IOE :> es) => Eff (Exit : es) a -> Eff es a
runExit =
  evalStaticRep
    Exit
      { exitWithHandler = SIO.exitWith
      , exitFailureHandler = SIO.exitFailure
      }

die :: (Exit :> es, Stdio :> es) => Eff (Error Builder : es) a -> Eff es a
die m = m !. (putStrLn .> exitFailure)
