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
  [ testCase "example"
      $ "case a0 :: Either _ _ of { Left a1 -> 0 ; Right a1 -> case a1 of {} }"
      @=? prettyFun_
        (Case "Either _ _" id "0"
          (Alt
            (Pat "Left" (Field (NoField (Const "0"))))
            (Pat "Right" (Field (NoField (Absurd id))))))
  ]
