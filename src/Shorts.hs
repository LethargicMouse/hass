{-# LANGUAGE ConstraintKinds #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Shorts where

import Control.Lens (Lens', over, set, view)
import Data.String (IsString (fromString))
import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Console.ByteString (Console, putStrLn, runConsole)
import Effectful.Dispatch.Static (SideEffects (WithSideEffects), StaticRep, evalStaticRep, getStaticRep, unsafeEff_)
import Effectful.State.Static.Local (State, gets, modify)
import qualified System.Exit as E
import Prelude hiding (putStrLn)

type Dies es = (Console :> es, Exit :> es)

die' :: (Dies es) => String -> Eff es a
die' e = putStrLn (fromString e) >> exitFailure

die :: (Show e, Dies es) => e -> Eff es a
die = die' . show

data Exit :: Effect

type instance DispatchOf Exit = Static WithSideEffects

newtype instance StaticRep Exit = Exit (forall a. IO a)

runDeath :: (IOE :> es) => Eff (Console : Exit : es) a -> Eff es a
runDeath = runExit . runConsole

runExit :: (IOE :> es) => Eff (Exit : es) a -> Eff es a
runExit = evalStaticRep (Exit E.exitFailure)

exitFailure :: (Exit :> es) => Eff es a
exitFailure = do
  Exit h <- getStaticRep
  unsafeEff_ h

use :: (State s :> es) => Lens' s a -> Eff es a
use = gets . view

assign :: (State s :> es) => Lens' s a -> a -> Eff es ()
assign l a = modify (set l a)

modifying :: (State s :> es) => Lens' s a -> (a -> a) -> Eff es ()
modifying l f = modify (over l f)

enclosed :: String -> String -> String
enclosed q s = q ++ s ++ q
