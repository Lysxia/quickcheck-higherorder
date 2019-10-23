{-# LANGUAGE
    FlexibleContexts,
    GADTs,
    ScopedTypeVariables,
    TypeOperators,
    UndecidableInstances #-}

module Test.QuickCheck.HigherOrder.Function where

import Data.Void
import Data.Kind (Type)

import Test.QuickCheck (Arbitrary(..))

import Test.QuickCheck.HigherOrder.Constructible

infixr 1 :->

type FunName = String
type TypeName = String
type ConName = String

newtype Repr_ a = Repr_ (Repr a)

instance Arbitrary (Repr a) => Arbitrary (Repr_ a) where
  arbitrary = Repr_ <$> arbitrary
  shrink (Repr_ r) = Repr_ <$> shrink r

data a :-> r where
  Const :: r -> a :-> r
  CoApply :: Constructible a => Repr_ a -> (b :-> (a -> b) :-> r) -> (a -> b) :-> r
  Apply :: FunName -> (a -> b) -> (b :-> r) -> (a :-> r)
  Case :: TypeName -> (a -> x) -> Branches x r -> r -> (a :-> r)
  CaseInteger :: TypeName -> (a -> Integer) -> Bin r -> r -> (a :-> r)
  Absurd :: Void :-> r

data Branches x r where
  Fail :: Branches x r
  Alt :: Branches x r -> Branches y r -> Branches (Either x y) r
  Pat :: ConName -> Fields x r -> Branches x r

-- Smart constructors to enforce some invariants

coapply :: Constructible a => Repr_ a -> (b :-> (a -> b) :-> r) -> (a -> b) :-> r
coapply _ (Const h) = h
coapply a h = CoApply a h

apply :: FunName -> (a -> b) -> (b :-> r) -> (a :-> r)
apply _ _ (Const r) = Const r
apply fn f h = Apply fn f h

case_ :: TypeName -> (a -> x) -> Branches x r -> r -> (a :-> r)
case_ _ _ Fail r = Const r
case_ tn f b r = Case tn f b r

caseInteger :: TypeName -> (a -> Integer) -> Bin r -> r -> (a :-> r)
caseInteger _ _ BinEmpty r = Const r
caseInteger tn f b r = CaseInteger tn f b r

alt :: Branches x r -> Branches y r -> Branches (Either x y) r
alt Fail Fail = Fail
alt b1 b2 = Alt b1 b2

data Fields x r where
  NoField :: r -> Fields () r
  Field :: Fields x (y :-> r) -> Fields (x, y) r

data Bin r
  = BinEmpty
  | BinAlt r (Bin r) (Bin r)

shrinkFun :: forall a r. (r -> [r]) -> (a :-> r) -> [a :-> r]
shrinkFun shrinkR = go where
  go :: forall b. (b :-> r) -> [b :-> r]
  go Absurd = []
  go (Const r) = fmap Const (shrinkR r)
  go (CoApply a h) = fmap (coapply a) (shrinkFun go h) ++ fmap (\a' -> CoApply a' h) (shrink a)
  go (Apply fn f h) = apply fn f <$> go h
  go (Case tn f b r) = Const r : fmap (\b' -> case_ tn f b' r) (shrinkBranches shrinkR b)
  go (CaseInteger tn f b r) = root b : fmap (\b' -> caseInteger tn f b' r) (shrinkBin shrinkR b) where
    root BinEmpty = Const r
    root (BinAlt r' _ _) = Const r'

shrinkBranches :: forall x r. (r -> [r]) -> Branches x r -> [Branches x r]
shrinkBranches shrinkR = go where
  go :: forall y. Branches y r -> [Branches y r]
  go Fail = []
  go (Alt b1 b2) = Fail : fmap (\b1' -> alt b1' b2) (go b1) ++ fmap (alt b1) (go b2)
  go (Pat cn d) = Fail : fmap (Pat cn) (shrinkFields shrinkR d)

shrinkFields :: forall x r. (r -> [r]) -> Fields x r -> [Fields x r]
shrinkFields shrinkR = go where
  go :: forall y. Fields y r -> [Fields y r]
  go (NoField r) = fmap NoField (shrinkR r)
  go (Field h) = fmap Field (shrinkFields (shrinkFun shrinkR) h)

shrinkBin :: forall r. (r -> [r]) -> Bin r -> [Bin r]
shrinkBin shrinkR = go where
  go BinEmpty = []
  go (BinAlt r b0 b1) =
    BinEmpty : b0 : b1
      :  fmap (\b0' -> BinAlt r b0' b1) (go b0)
      ++ fmap (\b1' -> BinAlt r b0 b1') (go b1)
      ++ fmap (\r' -> BinAlt r' b0 b1) (shrinkR r)
