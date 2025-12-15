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
f ?: a = runNonDet OnEmptyKeep f >>= either (const a) pure

(<.) :: (Monad m) => m b -> (t -> m a) -> t -> m b
m <. f = \x -> m << f x

(<<) :: (Monad m) => m b -> m a -> m b
b << a = a >> b

(!.) :: Eff (Error e : es) a -> (e -> Eff es a) -> Eff es a
m !. f = runErrorNoCallStackWith f m
