
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE InstanceSigs   #-}

module Fizzbuzz where

import           Control.Applicative
import           Data.Profunctor     (lmap, rmap)
import           Data.Void           (Void)
import           Function

data Fizzbuzz where
  Lit :: Int -> Fizzbuzz
  Fizz :: Fizzbuzz
  Buzz :: Fizzbuzz
  Fizzbuzz :: Fizzbuzz
  deriving (Show, Eq)

fizzbuzzCtx :: [String <--String--: Int]
fizzbuzzCtx = fmap (Context id) [1..100]

fizz :: Int -> Maybe Fizzbuzz
fizz i
  | i `mod` 3 == 0 = Just Fizz
  | otherwise = Nothing

buzz :: Int -> Maybe Fizzbuzz
buzz i
  | i `mod` 5 == 0 = Just Buzz
  | otherwise = Nothing

fizzbuzz :: Int -> Maybe Fizzbuzz
fizzbuzz i
  | i `mod` 15 == 0 = Just Fizzbuzz
  | otherwise = Nothing

lit :: Int -> Fizzbuzz
lit = Lit

fizzbuzzFun :: (r <--r--: Int) :--r--> Fizzbuzz
fizzbuzzFun = runContextFun (_cocurry fizzbuzz *> _cocurry fizz *> _cocurry buzz *> __cocurry lit)
  where
    _cocurry = ContextFun . cocurryMaybe . cps
    __cocurry = ContextFun . cocurryFun . cps

runFizzbuzz = fmap (\ctx -> runFun fizzbuzzFun ctx toStr) fizzbuzzCtx
  where
    toStr = \case
      Lit i    -> show i
      Fizz     -> "fizz"
      Buzz     -> "buzz"
      Fizzbuzz -> "fizzbuzz"

newtype ContextFun r a b c = ContextFun { runContextFun :: (c <--r--: b) :--r--> a }

instance Functor (ContextFun r a b) where
  fmap f (ContextFun h) = ContextFun $ lmap (lmap f) h

instance Applicative (ContextFun r a b) where
  pure :: x -> ContextFun r a b x
  pure x = ContextFun . Fun $ \(Context kx _) _ -> kx x

  liftA2 :: (x -> y -> z) -> ContextFun r a b x  -> ContextFun r a b y -> ContextFun r a b z
  liftA2 h (ContextFun f) (ContextFun g) = ContextFun . Fun $ \(Context kz b) ka ->
    runFun f (Context (\x -> runFun g (Context (kz . h x) b) ka) b) ka

instance Monad (ContextFun r a b) where
  (>>=) :: ContextFun r a b x -> (x -> ContextFun r a b y) -> ContextFun r a b y
  (ContextFun (Fun x)) >>= f =
    let run = runFun . runContextFun
    in ContextFun . Fun $ \(Context ky b) ka -> x (Context (\x' -> run (f x') (Context ky b) ka) b) ka
