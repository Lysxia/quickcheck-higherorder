# Higher-order QuickCheck

A QuickCheck extension for properties of higher-order functions.

## Summary

QuickCheck has a cute trick to automatically convert functions
`Thing -> Bool` or `Thing -> Gen Bool` to testable properties,
provided `Thing` is an instance of `Arbitrary` and `Show`,
i.e., there is a random generator, a shrinker, and a printer for `Thing`.
Sadly, those constraints limit the range of types that `Thing` can be.
In particular, they rule out functions, but also values of infinite size in
general.

The present library lifts that limitation, generalizing that technique to
arbitrary types, and provides other related quality-of-life improvements for
property-based test suites.

The key idea is to separate the `Thing` manipulated by the application under
test, of arbitrary structure, from its *representation*, which is manipulated
by QuickCheck and needs to be "concrete" enough to be possible to generate,
shrink, and show.

### Constructible types

The `Constructible` type class relates types `a` to representations
`Repr a` from which their values can be constructed.
Constraints for `Arbitrary` and `Show` are thus attached to those
representations instead of the raw type that will be used in properties.

```haskell
class (Arbitrary (Repr a), Show (Repr a)) => Constructible a where
  type Repr a :: Type
  fromRepr :: Repr a -> a
```

To illustrate what it enables, here's a higher-order property:

```haskell
prop_bool :: (Bool -> Bool) -> Bool -> Property
prop_bool f x =
  f (f (f x)) === f x
```

In vanilla QuickCheck, it needs a little wrapping:

```haskell
main :: IO ()
main = quickCheck (\(Fn f) x -> prop_bool f x)
```

The simpler `quickCheck prop_bool` would not typecheck because `Bool -> Bool`
is not an instance of `Arbitrary` nor `Show`.

With "higher-order" QuickCheck, that wrapping performed by `Fn` is instead
taken care of by the `Constructible` class, so we can write simply:

```haskell
main :: IO ()
main = quickCheck' prop_bool
```

### Testable equality

The `Eq` class is limited to types with *decidable* equality,
which typically requires them to have values of "finite size".
Most notably, the type of functions `a -> b` cannot be an instance of `Eq` in
general.

But testing can still be effective with a weaker constraint, dubbed
*testable equality*. To compare two functions, we can generate some
arguments and compare their images.
That is useful even if we can't cover the whole domain:

- if we find two inputs with distinct outputs, then the two functions
  are definitely not equal, and we now have a very concrete counterexample to
  contemplate;
- if we don't find any difference, then we can't conclude for sure, but:

    1. we can always try harder (more inputs, or rerun the whole property from
       scratch);
    2. in some situations, such as implementations of algebraic structures,
       bugs cause extremely obvious inequalities. If only we would look at them.
       The point of this new feature is to lower the bar for testing equations
       between higher-order values in the first place.

This package introduces a new type class `TestEq`, for testable equality.

```haskell
class TestEq a where
  (=?) :: a -> a -> Property
```

The codomain being `Property` offers some notable capabilities:

1. as explained earlier, we can use randomness to choose finite subsets
   of infinite values (such as functions) to compare;

2. we can also provide detailed context in the case of failure,
   by reporting the observations which lead to unequal outcomes.

For example, we can rewrite `prop_bool` as an algebraic property of functions
using `TestEq`:

```haskell
prop_bool :: (Bool -> Bool) -> Property
prop_bool f = (f . f . f) =? f
```

### More types of properties

Many common properties are quite simple, like `prop_bool`.
However, QuickCheck's way of declaring properties as functions with result type
`Property` introduces some unexpected complexity in the types.

For example, try generalizing the property `prop_bool` above to
arbitrary types instead of `Bool` (so it's no longer valid, of course).
Since we use testable equality of functions `a -> a`, we incur constraints that
the domain must be `Constructible`, and the codomain itself must have
testable equality.

```haskell
prop_fun :: (Constructible a, TestEq a) => (a -> a) -> Property
prop_fun f = (f . f . f) =? f
```

My debatable opinion is that this type tells both too much and too little.
Too much, because the constraints leak information about the very specific
way in which the comparison is performed. Too little, because a `Property`
can do a lot of things besides testing the equality of two values;
in fact that is one cause for the previous concern.

A more precise formulation:

```haskell
prop_fun :: (a -> a) -> Equation (a -> a)
prop_fun f = (f . f . f) :=: f
```

This does not actually do the comparison, but exposes just the necessary
amount of information to do it in whatever way one deems appropriate.
Indeed, `Equation` is a simple pair type:

```haskell
data Equation a = a :=: a
```

It is equipped with a `Testable` instance that will require the
`Constructible` and `TestEq` constraints, but that happens only when we run the
test with `quickCheck'`.

## Example

```haskell
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.HigherOrder (property', Equation((:=:)), CoArbitrary)

import Control.Monad.Cont (Cont, ContT(..), callCC)


-- Example property
callCC_bind :: forall r a. Cont r a -> Equation (Cont r a)
callCC_bind m = callCC ((>>=) m) :=: m


main :: IO ()
main = quickCheck' (callCC_bind @Int @Int)


-- Newtype boilerplate

import Test.QuickCheck (Gen)
import Test.QuickCheck.HigherOrder (CoArbitrary, TestEq(..), Constructible(..))

-- Constructible instances
instance (CoArbitrary Gen (m r), Constructible a, Constructible (m r)) => Constructible (ContT r m a) where
  type Repr (ContT r m a) = Repr ((a -> m r) -> m r)
  fromRepr = ContT . fromRepr

instance (TestEq ((a -> m r) -> m r)) => TestEq (ContT r m a) where
  ContT f =? ContT g = f =? g
```

## Features

- Redesigned `Testable`.
- A new type class `Constructible` for values with representations
  which can be generated randomly, shrunk, and shown.
- A new type class `TestEq` for *testable equality*.
- Representation of higher-order functions (via [*test-fun*](https://hackage.haskell.org/package/test-fun)).
- Replace property combinators with constructors: properties with
  observable syntax.
