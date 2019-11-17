{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.QuickCheck.HigherOrder.TestEq where

import Data.Functor.Identity
import qualified Data.Monoid as Monoid
import Test.QuickCheck

import Test.QuickCheck.HigherOrder.Constructible
import Test.QuickCheck.HigherOrder.Testable.Class

-- | Testable equality
class TestEq a where
  -- | A property that /probably/ fails if the two values are not equal.
  --
  -- @
  -- (a '=?' a)  =  'property' 'True'
  -- @
  (=?) :: a -> a -> Property

-- | Default method to convert 'Eq' (decidable equality) into 'TestEq'.
decEq :: (Eq a, Show a) => a -> a -> Property
decEq a b = a === b

infix 4 =?


-- 'TestEq' instances

instance (Constructible a, TestEq b) => TestEq (a -> b) where
  f =? g = property' (\a -> f a =? g a)

deriving instance TestEq a => TestEq (Identity a)

instance (TestEq a, TestEq b) => TestEq (a, b) where
  (a1, b1) =? (a2, b2) =
    counterexample "(_, _) =? (_, _)   (1) ..." (a1 =? a2) .&&.
    counterexample "(_, _) =? (_, _)   (2) ..." (b1 =? b2)

instance (TestEq a, TestEq b) => TestEq (Either a b) where
  Left a1  =? Left a2  = counterexample "Left _ =? Left _   ..."   $ a1 =? a2
  Right b1 =? Right b2 = counterexample "Right _ =? Right _   ..." $ b1 =? b2
  Left _ =? Right _ = counterexample "Left _ /= Right _" $ property False
  Right _ =? Left _ = counterexample "Right _ /= Left _" $ property False

instance TestEq a => TestEq (Maybe a) where
  Just a =? Just b = counterexample "Just _ =? Just _   ..." $ a =? b
  Nothing =? Nothing = property True
  Just _ =? Nothing = counterexample "Just _ /= Nothing" $ property False
  Nothing =? Just _ = counterexample "Nothing /= Just _" $ property False

instance TestEq a => TestEq [a] where
  [] =? [] = property True
  a : as =? b : bs =
    counterexample "(_ : _) =? (_ : _)   (1) ..." (a =? b) .&&.
    counterexample "(_ : _) =? (_ : _)   (2) ..." (as =? bs)
  [] =? _ : _ = counterexample "[] /= (_ : _)" $ property False
  _ : _ =? [] = counterexample "(_ : _) /= []" $ property False

instance TestEq Integer where (=?) = decEq
instance TestEq Int where (=?) = decEq
instance TestEq Word where (=?) = decEq
instance TestEq Double where (=?) = decEq
instance TestEq Char where (=?) = decEq
instance TestEq () where (=?) = decEq
instance TestEq Bool where (=?) = decEq
instance TestEq Ordering where (=?) = decEq

deriving instance TestEq a => TestEq (Monoid.Sum a)
