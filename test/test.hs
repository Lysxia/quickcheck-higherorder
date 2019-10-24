{-# LANGUAGE TypeApplications, TypeOperators #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.QuickCheck (Property, (===))

import Test.QuickCheck.HigherOrder
import Test.QuickCheck.HigherOrder.Function
import Test.QuickCheck.HigherOrder.Function.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
  [ testFunction
  ]

testFunction :: TestTree
testFunction = testGroup "Function"
  [ testFunctionPretty
  , testFunctionQC
  ]

prettyFun_ :: (a :-> String) -> String
prettyFun_ = prettyFun tConst

testFunctionPretty :: TestTree
testFunctionPretty = testGroup "pretty"
  [ testCase "case"
      $ "case a0 :: Either _ _ of { Left a1 -> 0 ; Right a1 -> case a1 of {} }"
      @=? prettyFun_
        (Case "Either _ _" id "0"
          (Alt
            (Pat "Left" (Field (NoField (Const "0"))))
            (Pat "Right" (Field (NoField (Absurd id))))))
  , testCase "coapply"
      $ "case a0 0 of {}"
      @=? prettyFun_ (CoApply (0 :: Int) id (Absurd id))
  , testCase "apply"
      $ "case f a0 of {}"
      @=? prettyFun_ (Apply "f" id (Absurd id))
  , testCase "case-Integer"
      $ "case a0 :: Integer of { -1 -> -1 ; 0 -> 0 ; 1 -> 1 ; _ -> 2 }"
      @=? prettyFun_
        (CaseInteger "Integer" id "2"
          (binAlt "0" (binAlt "1" BinEmpty BinEmpty) (binAlt "-1" BinEmpty BinEmpty)))
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
