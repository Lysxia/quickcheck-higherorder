{-# LANGUAGE TypeApplications, TypeOperators #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.QuickCheck (Property, (===))

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

testFunctionQC :: TestTree
testFunctionQC = testGroup "qc"
  [ testProperty "trivial-Int"    (property' (trivialProperty @Int))
  , testProperty "trivial-Bool"   (property' (trivialProperty @Bool))
  , testProperty "trivial-Either" (property' (trivialProperty @(Either () ())))
  , testProperty "bad-Int"    (property' (badProperty @Int))
  , testProperty "bad-Bool"   (property' (badProperty @Bool))
  , testProperty "bad-Either" (property' (badProperty @(Either () ())))
  ]
