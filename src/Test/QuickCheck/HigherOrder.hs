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

    -- * Runners
  , quickCheck'
  , quickCheckWith'

    -- * Helpers
  , forAll_
  , ok
  , ko

    -- * Testable equality
  , TestEq(..)
  , decEq

    -- * CoArbitrary
    --
    -- From "Test.Fun".
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
