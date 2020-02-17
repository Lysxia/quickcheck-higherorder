{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.QuickCheck.HigherOrder.Internal.Constructible where

import Data.Functor.Identity
import qualified Data.Monoid as Monoid

import Test.QuickCheck

-- * The 'Constructible' class

-- | A 'Constructible' type is associated with a type of "finite descriptions"
-- that can be generated, shown (e.g., as counterexamples in QuickCheck), and
-- interpreted as values.
--
-- N.B.: Not all values must have a description. We only expect the
-- descriptions to be sufficiently expressive for the purposes of testing.
--
-- The motivating example is the type of functions, which can be
-- finitely represented by the type @('Test.QuickCheck.Function.:->')@
-- (see "Test.QuickCheck.Function").
--
-- It turns out we can define 'Constructible' for anything
-- except 'IO' and higher-order functions (for now...).
--
-- An enhancement of 'Arbitrary' and 'Show' used by vanilla QuickCheck.
class (Arbitrary (Repr a), Show (Repr a)) => Constructible a where
  -- | The observable representation of a value.
  type Repr a
  type instance Repr a = a
  -- | Interpret a representation as a value.
  fromRepr :: Repr a -> a

-- * The 'Constructed' modifier

-- | 'Constructible' wrapper with 'Show' and 'Arbitrary' instances
-- that operate on the representation of the argument type.
--
-- Deconstruct with the 'Construct' pattern.
--
-- This is only useful for property combinators from vanilla QuickCheck, that
-- use the original 'Testable' class instead of
-- 'Test.QuickCheck.HigherOrder.Testable'' from this library.
data Constructed a = Constructed (Repr a) a

-- | A unidirectional pattern to deconstruct 'Constructed' values.
pattern Construct :: a -> Constructed a
pattern Construct a <- Constructed _ a

-- | A smart constructor for constructible values.
mkConstructed :: Constructible a => Repr a -> Constructed a
mkConstructed r = Constructed r (fromRepr r)

instance Constructible a => Arbitrary (Constructed a) where
  arbitrary = fmap mkConstructed arbitrary
  shrink (Constructed r _) = fmap mkConstructed (shrink r)

instance Constructible a => Show (Constructed a) where
  showsPrec n (Constructed r _) = showParen (n > 10) $
    showString "Constructed " . showsPrec 11 r . showString " _"


-- 'Constructible' instances

instance (CoArbitrary a, Function a, Show a, Constructible b) => Constructible (Fun a b) where
  type Repr (Fun a b) = Fun a (Repr b)
  fromRepr = fmap fromRepr

instance Constructible a => Constructible (Identity a) where
  type Repr (Identity a) = Repr a
  fromRepr = Identity . fromRepr

instance (Constructible a, Constructible b) => Constructible (a, b) where
  type Repr (a, b) = (Repr a, Repr b)
  fromRepr (a, b) = (fromRepr a, fromRepr b)

instance (Constructible a, Constructible b) => Constructible (Either a b) where
  type Repr (Either a b) = Either (Repr a) (Repr b)
  fromRepr (Left a) = Left (fromRepr a)
  fromRepr (Right b) = Right (fromRepr b)

instance Constructible a => Constructible (Maybe a) where
  type Repr (Maybe a) = Maybe (Repr a)
  fromRepr = fmap fromRepr

instance Constructible a => Constructible [a] where
  type Repr [a] = [Repr a]
  fromRepr = fmap fromRepr

instance Constructible Integer where fromRepr = id
instance Constructible Int where fromRepr = id
instance Constructible Word where fromRepr = id
instance Constructible Double where fromRepr = id
instance Constructible Char where fromRepr = id
instance Constructible () where fromRepr = id
instance Constructible Bool where fromRepr = id
instance Constructible Ordering where fromRepr = id

instance Constructible a => Constructible (Monoid.Sum a) where
  type Repr (Monoid.Sum a) = Monoid.Sum (Repr a)
  fromRepr = Monoid.Sum . fromRepr . Monoid.getSum
