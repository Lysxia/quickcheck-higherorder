{-# LANGUAGE PatternSynonyms #-}

-- | QuickCheck extension for properties of higher-order values.
--
-- See the README for an introduction.

module Test.QuickCheck.HigherOrder
  ( -- * Constructible values
    Constructible(..)
  , Constructed()
  , pattern Construct

    -- * Runners
  , quickCheck'
  , quickCheckWith'

    -- * Testable properties
  , Testable'(..)

    -- ** Types of testable properties
  , Equation(..)
  , Implication(..)
  , EqImpl

    -- ** Decidable properties
  , Decidable(..)

    -- * Testable equality
  , TestEq(..)
  , decEq

    -- * Helpers
  , forAll_
  , ok
  , ko

    -- * CoArbitrary
    -- | See also the documentation of "Test.Fun".
  , (:->)
  , applyFun
  , CoArbitrary(..)
  , cogenEmbed
  , cogenIntegral
  , coarbitraryGeneric
  ) where

import Test.Fun

import Test.QuickCheck.HigherOrder.Internal.Testable
import Test.QuickCheck.HigherOrder.Internal.Testable.Class
import Test.QuickCheck.HigherOrder.Internal.TestEq
import Test.QuickCheck.HigherOrder.Internal.Constructible
import Test.QuickCheck.HigherOrder.Internal.Function ()
