{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.QuickCheck.HigherOrder.Testable.Class where

import Test.QuickCheck

import Test.QuickCheck.HigherOrder.Constructible

-- * An alternative Testable

-- | Types that represent testable properties.
--
-- This is a clone of the 'Testable' class with an improved function instance.
class Testable' prop where
  property' :: prop -> Property

-- * Helpers

forAll_
  :: forall a prop
  .  (Constructible a, Testable' prop)
  => (a -> prop) -> Property
forAll_ f =
  forAllShrinkShow
    (arbitrary @(Repr a))
    (shrink    @(Repr a))
    (show      @(Repr a))
    (property' . f . fromRepr)

-- | A 'Property' is the canonical type of testable properties.
--
-- > @property' \@Property = property \@Property = id@
instance Testable' Property where
  property' = id

instance Testable' Bool where
  property' = property

-- | A generator represents a universally quantified property.
instance Testable' a => Testable' (Gen a) where
  property' = property . fmap property'

-- | A function represents a universally quantified property.
instance (Constructible a, Testable' b) => Testable' (a -> b) where
  property' = forAll_

