{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Combinators where

import Effectful (Eff)
import Effectful.Error.Static (Error, runErrorNoCallStackWith)
import Effectful.NonDet (NonDet, OnEmptyPolicy (OnEmptyKeep), runNonDet)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Local (State, evalState)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f .: g = \a b -> f (g a b)

infixl 0 -$
(-$) :: (a -> b -> c) -> b -> a -> c
f -$ x = flip f x

infixr 1 >>=>
(>>=>) :: (Monad m) => (x -> y -> m a) -> (a -> m b) -> x -> y -> m b
f >>=> g = \x y -> f x y >>= g

($$) :: Eff (Reader r : es) a -> r -> Eff es a
f $$ r = runReader r f

($~) :: Eff (State s : es) a -> s -> Eff es a
f $~ s = evalState s f

(?:) :: Eff (NonDet : es) a -> Eff es a -> Eff es a
m ?: a = either (const a) pure =<< runNonDet OnEmptyKeep m

(.>) :: (Monad m) => (t -> m a) -> m b -> t -> m b
f .> m = \x -> f x >> m

(!.) :: Eff (Error e : es) a -> (e -> Eff es a) -> Eff es a
m !. f = runErrorNoCallStackWith f m

(!:) :: Eff (Error e : es) a -> Eff es a -> Eff es a
m !: a = m !. const a

infixl 8 <.>
(<.>) :: (Functor f) => (b -> c) -> (a -> f b) -> a -> f c
f <.> g = fmap f . g
