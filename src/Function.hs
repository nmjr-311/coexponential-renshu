module Function where

import           Data.OpenUnion
import           Data.Profunctor   (Profunctor, dimap, rmap)
import           Type.Reflection   (Typeable)
import           TypeFun.Data.List (Delete)

newtype Fun r a b = Fun { runFun :: a -> (b -> r) -> r }

type a :-- r = Fun r a
type r --> b = r b
infixr 1 :--
infixr 0 -->

cps :: (a -> b) -> a :--r--> b
cps f = Fun $ flip ($) . f

cps' :: (a -> b) -> a :--r--> b
cps' f = Fun $ \a kb -> kb $ f a

($<<=) :: a :--r--> b -> a -> (b -> r) -> r
($<<=) = runFun

(<.>) :: (b :--r--> c) -> (a :--r--> b) -> a :--r--> c
(Fun g) <.> (Fun f) = Fun $ \a kc -> f a (`g` kc)

data Context r a b = Context (a -> r) b

type a <-- r = Context r a
type r --: b = r b
infixl 1 <--
infixl 0 --:

runContext :: (b <--r--: a) -> (a -> b) -> r
runContext (Context k x) f = k $ f x

trans :: ((b <--r--: a) -> r) -> a :--r--> b
trans c = Fun $ \a kb -> c $ Context kb a

-- a -> b => ¬ (a <--: b)
trans' :: (a -> b) -> ((b <--r--: a) -> r)
trans' = flip runContext

cocurry :: (a :--r--> Either b c) -> (b <--r--: a) :--r--> c
cocurry f = Fun $ \(Context kb a) kc -> runFun f a (either kb kc)

cocurry' :: (a :--r--> Either b c) -> (c <--r--: a) :--r--> b
cocurry' f = Fun $ \(Context kc a) kb -> runFun f a (either kb kc)

cocurryUnion :: Typeable x => (a -> Union xs) -> (x <--r--: a) :--r--> Union (Delete x xs)
cocurryUnion f = Fun $ \(Context kx a) k -> (kx @> k) $ f a

cocurryUnion' :: Typeable x => (a -> Union xs) -> (Union (Delete x xs) <--r--: a) :--r--> x
cocurryUnion' f = Fun $ \(Context k a) kx -> (kx @> k) $ f a

cocurryMaybe :: (a :--r--> Maybe b) -> (() <--r--: a) :--r--> b
cocurryMaybe f = cocurry $ rmap (maybe (Left ()) Right) f

cocurryFun :: (a :--r--> b) -> (r <--r--: a) :--r--> b
cocurryFun f = cocurry $ rmap Right f

uncocurry :: (b <--r--: a) :--r--> c -> a :--r--> Either b c
uncocurry (Fun f) = Fun $ \a kbc -> f (Context (kbc . Left) a) (kbc . Right)

coap :: a :--r--> Either (b <--r--: a) b
coap = Fun $ \a k -> k . Left $ Context (k . Right) a

instance Profunctor (Fun r) where
  dimap f g (Fun h) = Fun $ \ x k -> h (f x) (k . g)

instance Profunctor (Context r) where
  dimap f g (Context k x) = Context (k . f) (g x)

instance Semigroup b => Semigroup (a :--r--> b) where
  f <> g = Fun $ \a kb -> runFun f a (\b -> runFun g a (kb . (b <>)))

instance Monoid b => Monoid (a :--r--> b) where
  mempty = cps $ const mempty

instance Functor (Fun r x) where
  fmap f h = Fun $ \x kb -> runFun h x (kb . f)

instance Applicative (Fun r x) where
  pure = cps . const
  f <*> a = Fun $ \x kb -> runFun f x (\f' -> runFun a x (kb . f'))
