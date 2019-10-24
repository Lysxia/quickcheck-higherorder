{-# LANGUAGE TypeFamilies, TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Representation of (higher-order) functions.

module Test.QuickCheck.HigherOrder.Function
  ( (:->)
  , applyFun
  , CoArbitrary(..)
  , coarbitrarySynonym
  , coarbitraryIntegral
  , coarbitraryGeneric

  , prettyFun
  , shrinkFun
  , tConst
  ) where

import Test.QuickCheck (Arbitrary(..))

import Test.QuickCheck.HigherOrder.Function.Types ((:->), applyFun)
import Test.QuickCheck.HigherOrder.Function.CoArbitrary
import Test.QuickCheck.HigherOrder.Function.Pretty
import Test.QuickCheck.HigherOrder.Function.Shrink
import Test.QuickCheck.HigherOrder.Constructible

-- * Instances for @(:->)@

instance Show r => Show (a :-> r) where
  showsPrec _ h = s 0 where
    s = sparens 0 ("\\" ~% sVar defVar % " -> " ~% unExpr_ (tFun tShow h defCtx))

instance (CoArbitrary a, Arbitrary r) => Arbitrary (a :-> r) where
  arbitrary = coarbitrary arbitrary
  shrink = shrinkFun shrink

-- * 'Constructible' instance for @(->)@

instance (CoArbitrary a, Constructible b) => Constructible (a -> b) where
  type Repr (a -> b) = a :-> Repr b
  fromRepr h = fromRepr . applyFun h
