# Higher-order QuickCheck

A QuickCheck extension for properties of higher-order functions.

## Example

```haskell
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.HigherOrder (property', Equation((:=:)))

import Control.Monad.Cont (Cont, ContT(..), callCC)


-- Example property
callCC_bind :: forall r a. Cont r a -> Equation (Cont r a)
callCC_bind m = callCC ((>>=) m) :=: m


main :: IO ()
main = quickCheck (property' (callCC_bind @Int @Int))


-- Newtype boilerplate

-- Constructible instances
instance (CoArbitrary (m r), Constructible a, Constructible (m r)) => Constructible (ContT r m a) where
  type Repr (ContT r m a) = Repr ((a -> m r) -> m r)
  fromRepr = ContT . fromRepr

instance (TestEq ((a -> m r) -> m r)) => TestEq (ContT r m a) where
  ContT f =? ContT g = f =? g
```

## Features

- Redesigned `Testable`.
- A new `Constructible` type class for values with representations
  which can be generated randomly, shrunk, and shown.
- Representation of higher-order functions.
- Replace property combinators with constructors: properties with
  observable syntax.
