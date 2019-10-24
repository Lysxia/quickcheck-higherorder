{-# LANGUAGE TypeOperators #-}

module Test.QuickCheck.HigherOrder.Function.CoArbitrary where

import Data.Functor.Identity (Identity(..))
import Data.Void (Void)

import Test.QuickCheck (Gen)

import Test.QuickCheck.HigherOrder.Function.Types

-- * Random generation

-- A reimplementation of @CoArbitrary@ from QuickCheck,
-- but instead of functions we generate representations of functions.
class CoArbitrary a where
  coarbitrary :: Gen r -> Gen (a :-> r)

coarbitrarySynonym :: CoArbitrary b => FunName -> (a -> b) -> Gen r -> Gen (a :-> r)
coarbitrarySynonym fn f g = Apply fn f <$> coarbitrary g

coarbitraryIntegral :: TypeName -> (a -> Integer) -> Gen r -> Gen (a :-> r)
coarbitraryIntegral tn f g = CaseInteger tn f <$> g <*> genBin g

genBin :: Gen r -> Gen (Bin r)
genBin g = self where
  self = BinAlt <$> g <*> self <*> self

-- * Instances

instance CoArbitrary () where
  coarbitrary g = Const <$> g

instance CoArbitrary Void where
  coarbitrary _ = pure (Absurd id)

instance CoArbitrary Integer where
  coarbitrary = coarbitraryIntegral "Integer" id

instance CoArbitrary Int where
  coarbitrary = coarbitraryIntegral "Int" fromIntegral

instance CoArbitrary a => CoArbitrary (Identity a) where
  coarbitrary = coarbitrarySynonym "runIdentity" runIdentity
