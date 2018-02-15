-- | An alternative to Testable

module Test.QuickCheck.HigherOrder.Testable where

import Test.QuickCheck

import Test.QuickCheck.HigherOrder.Testable.Class
import Test.QuickCheck.HigherOrder.TestEq


-- * Syntax for properties


-- | Equation: an equals sign between two values.
data Equation a = (:=:) a a
  deriving (Eq, Ord, Show)

infix 5 :=:

instance TestEq a => Testable (Equation a) where
  property (a :=: b) = a =? b

instance TestEq a => Testable' (Equation a) where
  property' = property  -- the one defined just up there

instance Eq a => Decidable (Equation a) where
  decide (a :=: b) = a == b


-- | Expressions denoting a logical implication.
data Implication a b = (:==>) a b

infixr 2 :==>

-- | Implication between two equations.
type EqImpl a b = Implication (Equation a) (Equation b)

-- | Just use @('==>')@.
instance (Decidable a, Testable b) => Testable (Implication a b) where
  property (a :==> b) = decide a ==> b

-- | Just use @('==>')@.
instance (Decidable a, Testable' b) => Testable' (Implication a b) where
  property' (a :==> b) = decide a ==> property' b


-- | Decidable property.
class Decidable a where
  -- | The definition of decidability: we can compute whether a property is
  -- true.
  decide :: a -> Bool


-- * Auxiliary functions

-- | A named property that should pass.
ok :: Testable' prop => String -> prop -> (String, Property)
ok s prop = (s, property' prop)

-- | A named property that should fail.
ko :: Testable' prop => String -> prop -> (String, Property)
ko s = ok s . expectFailure . property'


-- Decidable instances

instance Decidable Bool where
  decide = id
