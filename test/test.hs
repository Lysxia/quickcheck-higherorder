{-# LANGUAGE TypeOperators #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Test.QuickCheck.HigherOrder.Function

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
  [ testFunction
  ]

testFunction :: TestTree
testFunction = testGroup "Function"
  [ testFunctionPretty
  ]

prettyFun_ :: (a :-> String) -> String
prettyFun_ = prettyFun eConst

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
      @=? prettyFun_ (CoApply (Repr_ 0 :: Repr_ Int) (Absurd id))
  , testCase "apply"
      $ "case f a0 of {}"
      @=? prettyFun_ (Apply "f" id (Absurd id))
  , testCase "case-Integer"
      $ "case a0 :: Integer of { -1 -> -1 ; 0 -> 0 ; 1 -> 1 ; _ -> 2 }"
      @=? prettyFun_
        (CaseInteger "Integer" id "2"
          (BinAlt "0" (BinAlt "1" BinEmpty BinEmpty) (BinAlt "-1" BinEmpty BinEmpty)))
  ]
