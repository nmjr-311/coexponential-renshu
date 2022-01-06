
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Function.Closed where

import           Function

class ClosedCategory f where
  type Unit f

  lift :: a -> f (Unit f) a
  lowwer :: f (Unit f) a -> a
  unit :: Unit f -> f x x
  split :: f y z -> f (f x y) (f x z)

class CoclosedCategory f where
  type Counit f

  colift :: f (Counit f) a -> a
  colowwer :: a -> f (Counit f) a
  counit :: f x x -> Counit f
  cosplit :: f (f x y) (f x z) -> f y z


instance ClosedCategory (->) where
  type Unit (->) = ()

  lift :: a -> () -> a
  lift = const

  lowwer :: (() -> a) -> a
  lowwer = ($ ())

  unit :: () -> a -> a
  unit = const id

  split :: (a -> b) -> (c -> a) -> c -> b
  split = (.)

instance CoclosedCategory (Context r) where
  type Counit (Context r) = r

  -- assume that only function from r to r is id
  colift :: (r <--r--: a) -> a
  colift (Context _ a) = a

  colowwer :: a -> (r <--r--: a)
  colowwer = Context id

  counit :: (a <--r--: a) -> r
  counit (Context k r) = k r

  cosplit :: ((a <--r--: b) <--r--: (a <--r--: c)) -> (b <--r--: c)
  cosplit (Context kctx (Context ka c)) = Context (kctx . Context ka) c
