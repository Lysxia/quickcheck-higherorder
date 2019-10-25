{-# LANGUAGE
    AllowAmbiguousTypes,
    FlexibleContexts,
    FlexibleInstances,
    GADTs,
    InstanceSigs,
    PolyKinds,
    ScopedTypeVariables,
    TypeApplications,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances #-}

module Test.QuickCheck.HigherOrder.Function.CoArbitrary where

import Control.Applicative (liftA2, liftA3)
import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import Data.Monoid (Sum(..))
import Data.Void (Void)
import GHC.Generics
import Type.Reflection

import Test.QuickCheck (Gen, arbitrary, listOf)

import Test.QuickCheck.HigherOrder.Constructible (Constructible(fromRepr))
import Test.QuickCheck.HigherOrder.Function.Types

-- * Random generation

-- A reimplementation of @CoArbitrary@ from QuickCheck,
-- but instead of functions we generate representations of functions.
class CoArbitrary a where
  coarbitrary :: Gen r -> Gen (a :-> r)

coarbitrarySynonym :: CoArbitrary b => FunName -> (a -> b) -> Gen r -> Gen (a :-> r)
coarbitrarySynonym fn f g = Apply fn f <$> coarbitrary g

coarbitraryIntegral :: TypeName -> (a -> Integer) -> Gen r -> Gen (a :-> r)
coarbitraryIntegral tn f g = liftA2 (CaseInteger tn f) g (genBin g)

genBin :: Gen r -> Gen (Bin r)
genBin g = self where
  self = liftA3 BinAlt (Just <$> g) self self

-- * Generic implementation

coarbitraryGeneric :: forall a r. (Generic a, GCoArbitrary a) => Gen r -> Gen (a :-> r)
coarbitraryGeneric = gcoarbitrary (shortTypeName @a) from

shortTypeName :: forall a. Typeable_ a => TypeName
shortTypeName = shortTypeName_ @_ @a ""

class (GCoArbitrary_ (Rep a), Typeable_ a) => GCoArbitrary a
instance (GCoArbitrary_ (Rep a), Typeable_ a) => GCoArbitrary a

class Typeable_ (a :: k) where
  shortTypeName_ :: String -> String

instance {-# OVERLAPPING #-} Typeable_ f => Typeable_ (f a) where
  shortTypeName_ = shortTypeName_ @_ @f . (' ' :) . ('_' :)

instance Typeable a => Typeable_ a where
  shortTypeName_ = ((++) . tyConName . typeRepTyCon) (typeRep @a)

class GCoArbitrary_ f where
  gcoarbitrary :: TypeName -> (a -> f p) -> Gen r -> Gen (a :-> r)

instance GGenBranches f => GCoArbitrary_ (M1 D c f) where
  gcoarbitrary tn f g = case ggenBranches g of
    SomeBranches f' gh -> liftA2 (Case tn (f' . unM1 . f)) g gh

-- ** Branches

data SomeBranches a r where
  SomeBranches :: (a -> x) -> Gen (Branches x r) -> SomeBranches a r

class GGenBranches f where
  ggenBranches :: Gen r -> SomeBranches (f p) r

instance (GGenBranches f, GGenBranches g) => GGenBranches (f :+: g) where
  ggenBranches g =
    case (ggenBranches g, ggenBranches g) of
      (SomeBranches fl gl, SomeBranches fr gr) ->
        let f (L1 x) = Left (fl x)
            f (R1 y) = Right (fr y)
        in SomeBranches f (liftA2 Alt gl gr)

instance (Constructor c, GGenFields f, GToList f) => GGenBranches (M1 C c f) where
  ggenBranches g = SomeBranches f gh where
    f = gToList () . unM1
    gh = Pat cn <$> ggenFields @f (fmap NoField) g
    cn = conName @c undefined

-- ** Fields

type family (>*>) (s :: Type) (f :: Type -> Type) :: Type where
  s >*> (f :*: g) = s >*> f >*> g
  s >*> M1 S c (K1 R a) = (s, a)
  s >*> U1 = s

infixl 9 >*>

type family (>->) (f :: Type -> Type) (r :: Type) :: Type where
  (f :*: g) >-> r = f >-> g >-> r
  M1 S c (K1 R a) >-> r = a :-> r
  U1 >-> r = r

infixr 9 >->

type GenField y r = Gen r -> Gen (Fields y r)

class GGenFields f where
  ggenFields :: forall y r. GenField y (f >-> r) -> GenField (y >*> f) r

instance (GGenFields f, GGenFields g) => GGenFields (f :*: g) where
  ggenFields :: forall y r. GenField y ((f :*: g) >-> r) -> GenField (y >*> (f :*: g)) r
  ggenFields = ggenFields @g @(y >*> f). ggenFields @f @y @(g >-> r)

instance CoArbitrary a => GGenFields (M1 S c (K1 R a)) where
  ggenFields y g = Field <$> y (coarbitrary g)

instance GGenFields U1 where
  ggenFields = id

class GToList f where
  gToList :: y -> f p -> (y >*> f)

instance (GToList f, GToList g) => GToList (f :*: g) where
  gToList y (u :*: v) = (y `gToList` u) `gToList` v

instance GToList (M1 S c (K1 R a)) where
  gToList y (M1 (K1 a)) = (y, a)

instance GToList U1 where
  gToList y _ = y

-- * Instances

instance (Constructible a, CoArbitrary b) => CoArbitrary (a -> b) where
  coarbitrary g = listOf arbitrary >>= go where
    go [] = Const <$> g
    go (x : xs) = CoApply x fromRepr <$> coarbitrary (go xs)

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

instance CoArbitrary Bool where
  coarbitrary = coarbitraryGeneric

instance CoArbitrary Ordering where
  coarbitrary = coarbitraryGeneric

instance CoArbitrary a => CoArbitrary [a] where
  coarbitrary = coarbitraryGeneric

instance CoArbitrary a => CoArbitrary (Maybe a) where
  coarbitrary = coarbitraryGeneric

instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (a, b) where
  coarbitrary = coarbitraryGeneric

instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (Either a b) where
  coarbitrary = coarbitraryGeneric

instance CoArbitrary a => CoArbitrary (Sum a) where
  coarbitrary = coarbitrarySynonym "getSum" getSum
