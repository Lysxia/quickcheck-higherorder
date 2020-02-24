-- | An alternative to Testable

module Test.QuickCheck.HigherOrder.Internal.Testable where

import Data.Traversable (for)
import Test.QuickCheck

import Test.QuickCheck.HigherOrder.Internal.Testable.Class
import Test.QuickCheck.HigherOrder.Internal.TestEq


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

-- | Variant of 'quickCheck' using the alternative 'Testable''.
quickCheck' :: Testable' prop => prop -> IO ()
quickCheck' = quickCheck . property'

-- | Variant of 'quickCheckWith' using the alternative 'Testable''.
quickCheckWith' :: Testable' prop => Args -> prop -> IO ()
quickCheckWith' args = quickCheckWith args . property'

-- | A named property that should __pass__.
--
-- Use 'ok' and 'ko' to construct lists of named properties
-- @[('String', 'Property')]@, which can be run using 'quickChecks',
-- or @testProperties@ from tasty-quickcheck.
ok :: Testable' prop => String -> prop -> (String, Property)
ok s prop = (s, property' prop)

-- | A named property that should __fail__.
--
-- See also 'ok'.
ko :: Testable' prop => String -> prop -> (String, Property)
ko s = ok s . expectFailure . property'

-- | Execute a list of named properties.
quickChecks :: [(String, Property)] -> IO Bool
quickChecks ps =
  fmap and . for ps $ \(name, p) -> do
    putStrLn ("=== " ++ name ++ " ===")
    r <- quickCheckResult p
    putStrLn ""
    return $ case r of
      Success{} -> True
      Failure{} -> False
      NoExpectedFailure{} -> False
      GaveUp{} -> False

-- Decidable instances

instance Decidable Bool where
  decide = id
