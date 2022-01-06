{-# LANGUAGE ScopedTypeVariables #-}

module Function.Adjunction () where

import           Data.Profunctor (lmap, rmap)
import           Function

-- adjunction
counit :: forall r a b. (a <--r--: Either b a) :--r--> b
counit = Fun $ \(Context ka e) kb -> either kb ka e

counit' :: forall r a b. (a <--r--: ((Either b a -> r) -> r)) :--r--> b
counit' = Fun $ \(Context ka ke) kb -> ke (either kb ka)

unit :: forall r a b. b :--r--> Either (a <--r--: b) a
unit = coap

_triangle1 :: forall r a x. (a <--r--: x) -> ((a <--r--: x) -> r) -> r
_triangle1 ctx@(Context ka x) kctx = kctx ctx -- identity
  where
    -- A <== ¬¬((A <== X) + A)
    unit' :: a <--r--: ((Either (a <--r--: x) a -> r) -> r)
    unit' = rmap (runFun (unit @r @a @x)) ctx
    comp0 :: r
    comp0 = runFun (counit' @r @a @(a <--r--: x)) unit' kctx

    -- unroll definition of unit' and coap
    comp1 :: r
    comp1 = runFun (counit' @r @a @(a <--r--: x)) (Context ka (\k -> k . Left $ Context (k. Right) x)) kctx

    -- unroll definiton of counit'
    comp3 :: r
    comp3 = (\k -> k . Left $ Context (k. Right) x) (either kctx ka)

    -- calc
    comp4 :: r
    comp4 = (either kctx ka) . Left $ Context (either kctx ka . Right) x

    -- simplify
    comp5 :: r
    comp5 = kctx $ Context ka x

    -- simplify
    comp6 :: r
    comp6 = kctx ctx

_triangle2 :: forall r a x. Either x a -> (Either x a -> r) -> r
_triangle2 eth k = k eth
  where
    cont0 :: Either (a <--r--: Either x a) a -> r
    cont0 = either (\ctx -> runFun (counit @r @a @x) ctx (k . Left)) (k . Right)
    comp0 :: r
    comp0 = runFun (unit @r @a @(Either x a)) eth cont0

    -- unroll definition of unit
    comp1 :: r
    comp1 = cont0 . Left $ Context (cont0 . Right) eth

    -- simplify `cont0 . Left` and `cont0 . Right`
    comp2 :: r
    comp2 = (\ctx -> runFun (counit @r @a @x) ctx (k . Left)) $ Context (k . Right) eth

    -- beta reduction
    comp3 :: r
    comp3 = runFun (counit @r @a @x) (Context (k . Right) eth) (k . Left)

    -- unroll definition of counit
    comp4 :: r
    comp4 = either (k . Left) (k . Right) eth

    -- simplify
    comp5 :: r
    comp5 = k eth
