{-# LANGUAGE ScopedTypeVariables #-}

module Function.Coexponential () where

import           Function (Context (Context), Fun (runFun), coap, cocurry,
                           type (--:), type (-->), type (:--), type (<--))

_commutative :: forall a b c r. (a :--r--> Either b c) -> a -> (Either b c -> r) -> r
_commutative f a k = runFun f a k
  where
    -- composition of coap and cocurry
    comp0 :: r
    comp0 = runFun coap a cont0
    cont0 :: Either (b <--r--: a) b -> r
    cont0 = \case
      Left ctx -> runFun (cocurry f) ctx (k . Right)
      Right b  -> k . Left $ b

    -- unroll definition of coap
    comp1 :: r
    comp1 = cont0 (Left $ Context (cont0 . Right) a)

    -- calculate `cont0 (Left ..)`
    comp2 :: r
    comp2 = runFun (cocurry f) (Context (cont0 . Right) a) (k . Right)

    -- calculate cocurry
    comp3 :: r
    comp3 = runFun f a (either (cont0 . Right) (k . Right))

    -- caclucate `cont0 . Right`
    comp4 :: r
    comp4 = runFun f a (either (k . Left) (k . Right))

    -- calculate `either (f . Left) (f . Right) = f`
    comp5 ::r
    comp5 = runFun f a k
