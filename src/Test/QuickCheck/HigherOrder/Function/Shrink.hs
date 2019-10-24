{-# LANGUAGE GADTs, ScopedTypeVariables, TypeOperators #-}

-- | Shrinker for representation of functions.

module Test.QuickCheck.HigherOrder.Function.Shrink where

import Test.QuickCheck (shrink)
import Test.QuickCheck.HigherOrder.Function.Types

shrinkFun :: forall a r. (r -> [r]) -> (a :-> r) -> [a :-> r]
shrinkFun shrinkR = go where
  go :: forall b. (b :-> r) -> [b :-> r]
  go (Absurd _) = []
  go (Const r) = fmap Const (shrinkR r)
  go (CoApply a f h) = fmap (coapply a f) (shrinkFun go h) ++ fmap (\a' -> CoApply a' f h) (shrink a)
  go (Apply fn f h) = apply fn f <$> go h
  go (Case tn f r b) = Const r : fmap (\b' -> case_ tn f r b') (shrinkBranches shrinkR b)
  go (CaseInteger tn f r b) = root b : fmap (\b' -> caseInteger tn f r b') (shrinkBin shrinkR b) where
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
