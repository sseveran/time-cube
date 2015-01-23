{-# OPTIONS -Wall #-}

-- |
-- Module      : Data.Time.Cube.Base
-- License     : BSD3
-- Maintainer  : ehaussecker@alphaheavy.com
-- Stability   : Experimental
-- Portability : GHC 7.8.* on Unix
--
-- Time cube test suite
module Main where

import Data.Time.Cube
import Foreign.C.Time
import GHC.TypeLits.SigNat
import Test.QuickCheck

main :: IO ()
main = return ()

-- |
-- Test the signat construction.
testSigNat :: Integer -> Bool
testSigNat n =
  case someSigNatVal n
    of SomeSigNat proxy1 ->
         case promoteSigNat proxy1 SomeSigNat
           of SomeSigNat proxy2 -> sigNatVal proxy2 == n