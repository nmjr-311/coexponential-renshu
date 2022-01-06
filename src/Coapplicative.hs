{-# LANGUAGE InstanceSigs #-}

module Coapplicative where

import           Data.OpenUnion    (Union, restrict)
import           Data.Profunctor   (Profunctor)
import           Function
import           Type.Reflection   (Typeable)
import           TypeFun.Data.List (Delete)

class Profunctor f => ContravariantCPS r f | f -> r where
  {-# MINIMAL contraCPS #-}
  contraCPS :: (a :--r--> b) -> f b c -> f a c
  (<#>) :: (a -> Either b c) -> f c d -> f (b <--r--: a) d
  f <#> a  = contraCPS (cocurry (cps f)) a
  (>#<) :: (a -> Either b c) -> f b d -> f (c <--r--: a) d
  f >#< a  = contraCPS (cocurry' (cps f)) a
  (<##>) :: Typeable x => (a -> Union xs) -> f x d -> f (Union (Delete x xs) <--r--: a) d
  f <##> a  = contraCPS (cocurryUnion' f) a

class ContravariantCPS r f => Coapply r f where
  -- copure :: a -> f a
  (<&>) :: f (b <--r--: a) c -> f b c -> f a c
  (<||>) :: Typeable x => f x b -> f (Union (Delete x xs)) b -> f (Union xs) b
  x <||> xs = restrict <#> x <&> xs

  divide :: (a -> Either b c) -> f b d -> f c d -> f a d
  divide f x y = f >#< x <&> y

  divide' :: Typeable x => (a -> Union xs) -> f x b -> f (Union (Delete x xs)) b -> f a b
  divide' f x y = f <##> x <&> y

instance ContravariantCPS r (Fun r) where
  contraCPS (Fun f) (Fun g) = Fun $ \a kx -> f a (`g` kx)
  (>#<) :: (a -> Either b c) -> b :--r--> d -> (c <--r--: a) :--r--> d
  f >#< (Fun g) = comp4
    where
      init = contraCPS (cocurry' (cps f)) (Fun g)
      comp1 = Fun $ \ctx k -> runFun (cocurry' (cps f)) ctx (`g` k)
      comp2 = Fun $ \ctx k -> (\(Context kc a) kb -> runFun (cps f) a (either kb kc)) ctx (`g` k)
      comp3 = Fun $ \(Context kc a) k -> runFun (cps f) a (either (`g` k) kc)
      comp4 = Fun $ \(Context kc a) k -> either (`g` k) kc (f a)

instance Coapply r (Fun r) where
  (<&>) :: ((b <--r--: a) :--r--> c) -> (b :--r--> c) -> (a :--r--> c)
  (Fun f) <&> (Fun g) = Fun $ \a kc -> f (Context (`g` kc) a) kc

infixl 3 <#>, >#<, <##>, <&>
infixr 3 <||>
