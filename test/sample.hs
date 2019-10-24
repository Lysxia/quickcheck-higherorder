{-# LANGUAGE
    TypeApplications,
    TypeOperators #-}

module Main where

import Data.Foldable (for_)

import Test.QuickCheck (Gen, Arbitrary, arbitrary, sample')

import Test.QuickCheck.HigherOrder.Function
import Test.QuickCheck.HigherOrder.Function.Types (truncateFun)
import Test.QuickCheck.HigherOrder.Function.Pretty (render)

genFun :: (CoArbitrary a, Arbitrary b) => Gen (a :-> b)
genFun = coarbitrary arbitrary

sample_ :: Show a => Gen a -> IO ()
sample_ g = do
  xs <- sample' g
  for_ xs $ \x -> putStrLn (render (show x))

main :: IO ()
main = do
  sample_ (truncateFun 4 id 33 <$> genFun @(Int -> Int) @Int)
  sample_ (truncateFun 4 id 33 <$> genFun @(Int -> Either () ()) @Int)
