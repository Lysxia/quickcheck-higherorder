{-# LANGUAGE PatternSynonyms #-}

module Test.QuickCheck.HigherOrder
  ( -- * Constructible values
    Constructible(..)
  , Constructed()
  , pattern Construct

    -- * Testable class
  , Testable'(..)
  , Equation(..)
  , Implication(..)
  , EqImpl
  , Decidable(..)

    -- * Helpers
  , forAll_
  , ok
  , ko

    -- * Testable equality
  , TestEq(..)
  , decEq
  ) where

import Test.QuickCheck.HigherOrder.Testable
import Test.QuickCheck.HigherOrder.Testable.Class
import Test.QuickCheck.HigherOrder.TestEq
import Test.QuickCheck.HigherOrder.Constructible