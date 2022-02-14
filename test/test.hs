{-# LANGUAGE ExplicitForAll, TypeApplications, TypeOperators #-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck (Property, (===), testProperty, expectFailure)

import Test.QuickCheck.HigherOrder

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
  [ testFunction
  ]

testFunction :: TestTree
testFunction = testGroup "Function"
  [ testFunctionQC
  ]

trivialProperty :: (Eq a, Show a) => ((a -> a) -> a) -> Property
trivialProperty f = f id === f id

badProperty :: (Eq a, Show a) => ((a -> a) -> a) -> (a -> a) -> Property
badProperty f g = expectFailure (f id === f g)

fmap_dot :: forall a b c. (b -> c) -> (a -> b) -> Equation (Maybe a -> Maybe c)
fmap_dot g f = (fmap g . fmap f) :=: fmap (g . f)

testFunctionQC :: TestTree
testFunctionQC = testGroup "qc"
  [ testProperty "trivial-Int"    (property' (trivialProperty @Int))
  , testProperty "trivial-Bool"   (property' (trivialProperty @Bool))
  , testProperty "trivial-Either" (property' (trivialProperty @(Either () ())))
  , testProperty "bad-Int"    (property' (badProperty @Int))
  , testProperty "bad-Bool"   (property' (badProperty @Bool))
  , testProperty "bad-Either" (property' (badProperty @(Either () ())))
  , testProperty "fmap-dot"   (property' (fmap_dot @Int @Int @Int))
  ]
