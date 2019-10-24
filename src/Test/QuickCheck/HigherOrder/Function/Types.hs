{-# LANGUAGE
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
  Alt :: Branches x r -> Branches y r -> Branches (Either x y) r
  Pat :: ConName -> Fields x r -> Branches x r

-- | Representation of one branch of a @case@.
data Fields x r where
  NoField :: r -> Fields () r
  Field :: Fields x (y :-> r) -> Fields (x, y) r

-- | Representation of branches of a @case@ on an @Integer@.
data Bin r
  = BinEmpty
  | BinAlt r (Bin r) (Bin r)

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
