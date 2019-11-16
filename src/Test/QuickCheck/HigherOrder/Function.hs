{-# LANGUAGE
    FlexibleContexts,
    MultiParamTypeClasses,
    ScopedTypeVariables,
    TypeFamilies,
    TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Representation of (higher-order) functions.

module Test.QuickCheck.HigherOrder.Function where

import Test.Fun ((:->), applyFun, shrinkFun, cogenFun, CoArbitrary(..), Concrete(..))
import Test.QuickCheck (Arbitrary(..), Gen, choose)

import Test.QuickCheck.HigherOrder.Constructible

-- * Instances for @(:->)@

concrete :: (Arbitrary a, Show a) => Concrete a
concrete = Concrete shrink showsPrec

instance (CoArbitrary Gen a, Arbitrary r) => Arbitrary (a :-> r) where
  arbitrary = coarbitrary arbitrary
  shrink = shrinkFun shrink

instance (Constructible a, CoArbitrary Gen b) => CoArbitrary Gen (a -> b) where
  coarbitrary g = do
    n <- geometric 4
    cogenFun n concrete arbitrary fromRepr coarbitrary g

geometric :: Int -> Gen Int
geometric w = go 0 where
  go i = i `seq` do
    x <- choose (0, w)
    if x == 0 then
      pure i
    else
      go (i + 1)

-- * 'Constructible' instance for @(->)@

instance (CoArbitrary Gen a, Constructible b) => Constructible (a -> b) where
  type Repr (a -> b) = a :-> Repr b
  fromRepr h = fromRepr . applyFun h
