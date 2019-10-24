{-# LANGUAGE
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    EmptyCase,
    FlexibleContexts,
    FlexibleInstances,
    GADTs,
    ScopedTypeVariables,
    TypeOperators,
    UndecidableInstances #-}

-- | Representation of (higher-order) functions.
--
-- Instances for @(':->')@ can be found in "Test.QuickCheck.HigherOrder.Function".

module Test.QuickCheck.HigherOrder.Function.Types where

import Data.Void (Void)

import Test.QuickCheck (Arbitrary(..))

infixr 1 :->

type FunName = String
type TypeName = String
type ConName = String

class (Arbitrary r, Show r) => ConstructibleRepr r
instance (Arbitrary r, Show r) => ConstructibleRepr r

-- | Representation of functions @(a -> r)@.
data a :-> r where
  Const :: r -> a :-> r
  CoApply :: ConstructibleRepr w => w -> (w -> a) -> (b :-> (a -> b) :-> r) -> (a -> b) :-> r
  Apply :: FunName -> (a -> b) -> (b :-> r) -> (a :-> r)
  Case :: TypeName -> (a -> x) -> r -> Branches x r -> (a :-> r)
  CaseInteger :: TypeName -> (a -> Integer) -> r -> Bin r -> (a :-> r)
  Absurd :: (a -> Void) -> a :-> r

-- | Representation of the branches of a @case@.
data Branches x r where
  Fail :: Branches x r
  Alt :: !(Branches x r) -> !(Branches y r) -> Branches (Either x y) r
  Pat :: ConName -> !(Fields x r) -> Branches x r

-- | Representation of one branch of a @case@.
data Fields x r where
  NoField :: r -> Fields () r
  Field :: !(Fields x (y :-> r)) -> Fields (x, y) r

-- | Representation of branches of a @case@ on an @Integer@.
data Bin r
  = BinEmpty
  | BinAlt r (Bin r) (Bin r)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- Smart constructors to enforce some invariants

coapply :: ConstructibleRepr w => w -> (w -> a) -> (b :-> (a -> b) :-> r) -> (a -> b) :-> r
coapply _ _ (Const h) = h
coapply w f h = CoApply w f h

apply :: FunName -> (a -> b) -> (b :-> r) -> (a :-> r)
apply _ _ (Const r) = Const r
apply fn f h = Apply fn f h

case_ :: TypeName -> (a -> x) -> r -> Branches x r -> (a :-> r)
case_ _ _ r Fail = Const r
case_ tn f r b = Case tn f r b

caseInteger :: TypeName -> (a -> Integer) -> r -> Bin r -> (a :-> r)
caseInteger _ _ r BinEmpty = Const r
caseInteger tn f r b = CaseInteger tn f r b

alt :: Branches x r -> Branches y r -> Branches (Either x y) r
alt Fail Fail = Fail
alt b1 b2 = Alt b1 b2

--

applyFun :: (a :-> r) -> a -> r
applyFun (Const r) _ = r
applyFun (CoApply w f h) x = applyFun (applyFun h (x (f w))) x
applyFun (Apply _ f h) x = applyFun h (f x)
applyFun (Case _ f r b) x = applyBranches r b (f x)
applyFun (CaseInteger _ f r b) x = applyBin r b (f x)
applyFun (Absurd f) x = case f x of {}

applyBranches :: r -> Branches x r -> x -> r
applyBranches r Fail _ = r
applyBranches r (Alt b1 _) (Left  x) = applyBranches r b1 x
applyBranches r (Alt _ b2) (Right y) = applyBranches r b2 y
applyBranches _ (Pat _ d) x = applyFields d x

applyFields :: Fields x r -> x -> r
applyFields (NoField h) _ = h
applyFields (Field h) (x, y) = applyFun (applyFields h x) y

applyBin :: r -> Bin r -> Integer -> r
applyBin r BinEmpty _ = r
applyBin r (BinAlt r0 b0 b1) x = case compare x 0 of
  EQ -> r0
  GT -> applyBin' r b0 (x - 1)
  LT -> applyBin' r b1 (- x - 1)

applyBin' :: r -> Bin r -> Integer -> r
applyBin' r BinEmpty _ = r
applyBin' r (BinAlt r0 b0 b1) x
  | x == 0 = r0
  | x `div` 2 == 0 = applyBin' r b0 (x `div` 2)
  | otherwise      = applyBin' r b1 (x `div` 2)

--

instance Functor ((:->) a) where
  fmap g h0 = case h0 of
    Const r -> Const (g r)
    Apply fn f h -> Apply fn f (fmap g h)
    CoApply w f h -> CoApply w f (fmap (fmap g) h)
    Case tn f r b -> Case tn f (g r) (fmap g b)
    CaseInteger tn f r b -> CaseInteger tn f (g r) (fmap g b)
    Absurd f -> Absurd f

instance Functor (Branches x) where
  fmap g b = case b of
    Fail -> Fail
    Alt b1 b2 -> Alt (fmap g b1) (fmap g b2)
    Pat cn d -> Pat cn (fmap g d)

instance Functor (Fields x) where
  fmap g d = case d of
    NoField h -> NoField (g h)
    Field h -> Field (fmap (fmap g) h)

instance Foldable ((:->) a) where
  foldMap foldR h0 = case h0 of
    Const r -> foldR r
    Apply _ _ h -> foldMap foldR h
    CoApply _ _ h -> foldMap (foldMap foldR) h
    Case _ _ r b -> foldMap foldR b <> foldR r
    CaseInteger _ _ r b -> foldMap foldR b <> foldR r
    Absurd _ -> mempty

instance Foldable (Branches x) where
  foldMap foldR b = case b of
    Fail -> mempty
    Alt b1 b2 -> foldMap foldR b1 <> foldMap foldR b2
    Pat _ d -> foldMap foldR d

instance Foldable (Fields x) where
  foldMap foldR d = case d of
    NoField h -> foldR h
    Field h -> foldMap (foldMap foldR) h

truncateFun :: Int -> (r -> t) -> t -> (a :-> r) -> (a :-> t)
truncateFun 0 _ s _ = Const s
truncateFun n truncateR r h0 = case h0 of
  Const s -> Const (truncateR s)
  Apply fn f h -> Apply fn f (truncateFun (n-1) truncateR r h)
  CoApply w f h -> CoApply w f (truncateFun (n-1) (truncateFun (n-1) truncateR r) (Const r) h)
  Case tn f s b -> Case tn f (truncateR s) (fmap truncateR b)
  CaseInteger tn f s b -> CaseInteger tn f (truncateR s) (truncateBin (n-1) truncateR b)
  Absurd f -> Absurd f

truncateBin :: Int -> (r -> s) -> Bin r -> Bin s
truncateBin 0 _ _ = BinEmpty
truncateBin n truncateR d = case d of
  BinEmpty -> BinEmpty
  BinAlt r d0 d1 -> BinAlt (truncateR r) (go d0) (go d1)
    where go = truncateBin (n-1) truncateR
