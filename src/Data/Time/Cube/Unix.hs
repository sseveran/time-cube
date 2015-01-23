{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall                       #-}

-- |
-- Module      : Data.Time.Cube.Unix
-- License     : BSD3
-- Maintainer  : ehaussecker@alphaheavy.com
-- Stability   : Experimental
-- Portability : GHC 7.8.* on Unix
--
-- Unix timestamps.
module Data.Time.Cube.Unix (

 -- ** Unix Timestamps
       UnixDate(..)
     , UnixDateTime(..)
     , UnixDateTimeNanos(..)

     ) where

import Control.DeepSeq     (NFData(..))
import Data.Int            (Int32, Int64)
import Data.Time.Cube.Base (Calendar)
import Foreign.Ptr         (plusPtr)
import Foreign.Storable    (Storable(..))
import GHC.Generics        (Generic)

-- |
-- Days since Unix epoch.
newtype UnixDate (cal :: Calendar) = UnixDate Int32
   deriving (Eq, Generic, NFData, Ord, Storable)

-- |
-- Seconds since Unix epoch (excluding leap seconds).
newtype UnixDateTime (cal :: Calendar) = UnixDateTime Int64
   deriving (Eq, Generic, NFData, Ord, Storable)

-- |
-- Nanoseconds since Unix epoch (excluding leap seconds).
data UnixDateTimeNanos (cal :: Calendar) =
     UnixDateTimeNanos {-# UNPACK #-} !Int64 {-# UNPACK #-} !Int32
     deriving (Eq, Generic, Ord)

instance NFData (UnixDateTimeNanos cal) where

   rnf (UnixDateTimeNanos base nsec) = rnf base `seq` rnf nsec `seq` ()

instance Storable (UnixDateTimeNanos cal) where
   sizeOf  _ = 12
   alignment = sizeOf
   peekElemOff ptr n = do
       let off = 12 * n
       base <- peek . plusPtr ptr $ off
       nsec <- peek . plusPtr ptr $ off + 8
       return $! UnixDateTimeNanos base nsec
   pokeElemOff ptr n (UnixDateTimeNanos base nsec) = do
       let off = 12 * n
       flip poke base . plusPtr ptr $ off
       flip poke nsec . plusPtr ptr $ off + 8