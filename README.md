# Higher-order QuickCheck [![Hackage](https://img.shields.io/hackage/v/quickcheck-higherorder.svg)](https://hackage.haskell.org/package/quickcheck-higherorder) ![haskell-ci](https://github.com/Lysxia/quickcheck-higherorder/actions/workflows/haskell-ci.yml/badge.svg)

A QuickCheck extension for properties of higher-order values.

## Examples

Higher-order properties are properties which may:

1. quantify over functions;
2. state equalities between functions.

Some examples:

```haskell
fmap_dot :: forall a b c. (b -> c) -> (a -> b) -> Equation (Maybe a -> Maybe c)
fmap_dot g f x = (fmap g . fmap f) :=: fmap (f . g)

callCC_bind :: forall r a. Cont r a -> Equation (Cont r a)
callCC_bind m = callCC ((>>=) m) :=: m
```

*quickcheck-higherorder* makes it easy to define and test such properties.

```haskell
main :: IO ()
main = do
  quickCheck' (fmap_dot @Int @Int @Int)
  quickCheck' (callCC_bind @Int @Int)
```

(Additional setup is required for the `callCC` example.)

## Summary

QuickCheck has a cute trick to implicitly convert functions
`Thing -> Bool` or `Thing -> Gen Bool` to testable properties,
provided `Thing` is an instance of `Arbitrary` and `Show`,
i.e., there is a random generator, a shrinker, and a printer for `Thing`.
Sadly, those constraints limit the range of types that `Thing` can be.
In particular, they rule out functions and other values of "infinite size".

This library, *quickcheck-higherorder*, lifts that limitation, generalizing
that technique to arbitrary types, and provides other related quality-of-life
improvements for property-based test suites.

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

To illustrate what it enables, here's an example of higher-order property:

```haskell
prop_bool :: (Bool -> Bool) -> Bool -> Property
prop_bool f x =
  f (f (f x)) === f x
```

In vanilla QuickCheck, it needs a little wrapping to actually run it:

```haskell
main :: IO ()
main = quickCheck (\(Fn f) x -> prop_bool f x)
```

The simpler expression `quickCheck prop_bool` would not typecheck
because `Bool -> Bool` is not an instance of `Show`.

With "higher-order" QuickCheck, that wrapping performed by `Fn` is instead
taken care of by the `Constructible` class, so we can write simply:

```haskell
main :: IO ()
main = quickCheck' prop_bool
```

This is especially convenient when the function type is not
directly exposed in the type of the property (as in `prop_bool`),
but may be hidden inside various data types or newtypes.

### Testable equality

In a similar vein, the `Eq` class is limited to types with
*decidable equality*,
which typically requires them to have values of "finite size".
Most notably, the type of functions `a -> b` cannot be an instance of `Eq` in
general.

But testing can still be effective with a weaker constraint, dubbed
*testable equality*. To compare two functions, we can generate some random
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
arbitrary types instead of `Bool`.
Since we use testable equality of functions `a -> a`, we incur constraints that
the domain must be `Constructible`, and the codomain itself must have
testable equality.

```haskell
prop_fun :: (Constructible a, TestEq a) => (a -> a) -> Property
prop_fun f = (f . f . f) =? f
```

This type tells us both too much and too little.
Too much, because the constraints leak details about the very specific
way in which the comparison is performed. Too little, because a `Property`
can do a lot of things besides testing the equality of two values;
in fact that is one cause for the previous concern.

A more precise formulation is the following:

```haskell
prop_fun :: (a -> a) -> Equation (a -> a)
prop_fun f = (f . f . f) :=: f
```

This does not actually do the comparison, but exposes just the necessary
amount of information to do it in whatever way one deems appropriate.
Indeed, `Equation` is simply a type of pairs:

```haskell
data Equation a = a :=: a
```

It is equipped with a `Testable` instance that will require a `TestEq`
constraint indirectly at call sites only.

## Full example

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
