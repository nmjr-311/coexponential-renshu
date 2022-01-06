{-# LANGUAGE ScopedTypeVariables #-}

module Printer where

import           Coapplicative
import           Data.List          (uncons)
import           Data.List.NonEmpty (nonEmpty, toList)
import           Data.OpenUnion
import           Data.Profunctor    (Profunctor (lmap, rmap))
import           Function
import           GHC.Generics       (Generic)
import           Type.Reflection    (Typeable)
import           TypeFun.Data.List  (Delete)

class Monoid d => Document d where
  {-# MINIMAL char #-}
  char :: Char -> d

  string :: String -> d
  string = foldMap char

  int :: Int -> d
  int = string . show

  enclose :: d -> d -> d -> d
  enclose l r c = l <> c <> r

  brackets :: d -> d
  brackets = enclose (char '[') (char ']')

  braces :: d -> d
  braces = enclose (char '{') (char '}')

  quoted :: d -> d
  quoted = enclose (char '"') (char '"')

instance Document b => Document (a -> b) where
  char :: Char -> a -> b
  char = const . char

instance Document b => Document (a :--r--> b) where
  char c = cps . const $ char c

instance Document String where
  char c = [c]

newtype Printer r a b = Printer (a :--r--> b)
  deriving (Functor, Applicative, Profunctor, Semigroup, Monoid, Document, ContravariantCPS r, Coapply r)

runPrinter :: Printer r a r -> a -> r
runPrinter (Printer p) = ($ id) . (p $<<=)

printer :: (a -> b) -> Printer r a b
printer = Printer . cps

list :: Document b => Printer r a b -> Printer r [a] b
list p = brackets go
  where
    maybeToEither = maybe (Left ()) Right
    go = (maybeToEither . uncons) >#< mempty <&> (lmap fst p <> lmap snd tail')
    tail' = (fmap toList . maybeToEither . nonEmpty) >#< mempty <&> string ", " <> go

field :: Document b => String -> b
field f = quoted (string f) <> string ": "

data User = User { name :: String, age :: Int, friends :: [User] } deriving (Show, Eq, Typeable)

userPrinter :: forall r b. Document b => Printer r User b
userPrinter = braces $ namePrinter <> agePrinter <> friendPrinter
  where
    namePrinter = lmap name . printer $ \name' -> field "name" <> quoted (string name') <> char ','
    agePrinter = lmap age . printer $ \age' -> field "age" <> int age' <> char ','
    friendPrinter = lmap friends $ field "friends" <> list userPrinter

testUser :: User
testUser = User {
  name = "user1",
  age = 22,
  friends = [ User "user2" 25 [], User "user3" 18 [ User "user2" 25 []] ]
}

newtype Foo = Foo { foo :: String } deriving (Eq, Show, Typeable)
newtype Bar = Bar { bar :: String } deriving (Eq, Show, Typeable)

fooPrinter :: Document b => Printer r Foo b
fooPrinter = braces . lmap foo . printer $ \foo' -> field "foo" <> string foo'

barPrinter :: Document b => Printer r Bar b
barPrinter = braces . lmap bar . printer $ \foo' -> field "bar" <> string foo'

myPrinter :: Document b => Printer r (Union '[User, Foo, Bar]) b
myPrinter = userPrinter <||> fooPrinter <||> barPrinter <||> printer typesExhausted

testFoo :: Foo
testFoo = Foo "foo!!!!"

testBar :: Bar
testBar = Bar "bar!!!!"
