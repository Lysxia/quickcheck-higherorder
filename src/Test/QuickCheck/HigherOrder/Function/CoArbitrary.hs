{-# LANGUAGE TypeOperators #-}

module Test.QuickCheck.HigherOrder.Function.CoArbitrary where

import Data.Void (Void)

import Test.QuickCheck (Gen)

import Test.QuickCheck.HigherOrder.Function.Types

-- * Random generation

-- A reimplementation of @CoArbitrary@ from QuickCheck,
-- but instead of functions we generate representations of functions.
class CoArbitrary a where
  coarbitrary :: Gen r -> Gen (a :-> r)

instance CoArbitrary () where
  coarbitrary g = Const <$> g

instance CoArbitrary Void where
  coarbitrary _ = pure (Absurd id)

instance CoArbitrary Integer where
  coarbitrary = genIntegral "Integer" id

instance CoArbitrary Int where
  coarbitrary = genIntegral "Int" fromIntegral

genIntegral :: TypeName -> (a -> Integer) -> Gen r -> Gen (a :-> r)
genIntegral tn f g = CaseInteger tn f <$> g <*> genBin g

genBin :: Gen r -> Gen (Bin r)
genBin g = self where
  self = BinAlt <$> g <*> self <*> self

