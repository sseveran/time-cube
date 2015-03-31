{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# OPTIONS -Wall                      #-}

-- |
-- Module      : GHC.TypeLits.SigNat
-- License     : BSD3
-- Maintainer  : ehaussecker@alphaheavy.com
-- Stability   : Experimental
-- Portability : GHC 7.8.* on Unix
--
-- This module provides support for signed type-level naturals.
module GHC.TypeLits.SigNat (

 -- ** Signed Type-Level Naturals
       SigNat(..)
     , KnownSigNat(..)
     , SomeSigNat(..)
     , someSigNatVal
     , promoteSigNat

     ) where

import Data.Maybe (fromJust)
import Data.Proxy (Proxy(..))
import GHC.TypeLits

-- |
-- This is the kind of a signed type-level natural.
data SigNat where
     Plus  :: Nat -> SigNat
     Minus :: Nat -> SigNat

-- |
-- This class gives the integer associated with a signed type-level natural.
class KnownSigNat (signat :: SigNat) where

   sigNatVal :: Proxy signat -> Integer

instance KnownNat nat => KnownSigNat (Plus nat) where

   sigNatVal _ = natVal (Proxy :: Proxy nat)

instance KnownNat nat => KnownSigNat (Minus nat) where

   sigNatVal _ = negate $ natVal (Proxy :: Proxy nat)

-- |
-- This type represents an unknown signed type-level natural.
data SomeSigNat = forall signat . KnownSigNat signat => SomeSigNat (Proxy signat)

instance Eq SomeSigNat where

  SomeSigNat proxy1 == SomeSigNat proxy2 = sigNatVal proxy1 == sigNatVal proxy2

deriving instance Show SomeSigNat

-- |
-- Convert an integer into an unknown signed type-level natural.
someSigNatVal :: Integer -> SomeSigNat
someSigNatVal x =
  case fromJust . someNatVal $ abs x
    of SomeNat (_ :: Proxy nat) ->
         if x >= 0
         then SomeSigNat (Proxy :: Proxy (Plus  nat))
         else SomeSigNat (Proxy :: Proxy (Minus nat))

-- |
-- Promote a signed type-level natural.
promoteSigNat :: forall proxy signat a . KnownSigNat signat => proxy signat -> (Proxy signat -> a) -> a
promoteSigNat _ f = f Proxy