{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS -Wall               #-}

-- |
-- Module      : Data.Time.Cube.Local
-- License     : BSD3
-- Maintainer  : ehaussecker@alphaheavy.com
-- Stability   : Experimental
-- Portability : GHC 7.8.* on Unix
--
-- Local timestamps.
module Data.Time.Cube.Local (

 -- ** Local Timestamps
       LocalDate(..)
     , LocalDateTime(..)
     , LocalDateTimeNanos(..)

     ) where

import Control.DeepSeq     (NFData(..))
import Data.Ord            (comparing)
import Data.Time.Cube.Base (Calendar(..))
import Data.Time.Cube.UTC
import Data.Time.Cube.Zone (SomeOffset, TimeZone)
import Foreign.Ptr         (plusPtr)
import Foreign.Storable    (Storable(..))
import GHC.Generics        (Generic)

-- |
-- Local days since Unix epoch.
data LocalDate (cal :: Calendar) tz =
     LocalDate {-# UNPACK #-} !(UTCDate cal) {-# UNPACK #-} !(TimeZone tz)
     deriving Generic

-- |
-- Local seconds since Unix epoch (including leap seconds).
data LocalDateTime (cal :: Calendar) tz =
     LocalDateTime {-# UNPACK #-} !(UTCDateTime cal) {-# UNPACK #-} !(TimeZone tz)
     deriving Generic

-- |
-- Local nanoseconds since Unix epoch (including leap seconds).
data LocalDateTimeNanos (cal :: Calendar) tz =
     LocalDateTimeNanos {-# UNPACK #-} !(UTCDateTimeNanos cal) {-# UNPACK #-} !(TimeZone tz)
     deriving Generic

deriving instance (Eq tz, Eq (TimeZone tz)) => Eq (LocalDate cal tz)

deriving instance (Eq tz, Eq (TimeZone tz)) => Eq (LocalDateTime cal tz)

deriving instance (Eq tz, Eq (TimeZone tz)) => Eq (LocalDateTimeNanos cal tz)

instance (Eq tz, Eq (TimeZone tz)) => Ord (LocalDate cal tz) where

   compare = comparing $ \ (LocalDate date _) -> date

instance (Eq tz, Eq (TimeZone tz)) => Ord (LocalDateTime cal tz) where

   compare = comparing $ \ (LocalDateTime time _) -> time

instance (Eq tz, Eq (TimeZone tz)) => Ord (LocalDateTimeNanos cal tz) where

   compare = comparing $ \ (LocalDateTimeNanos time _) -> time

instance NFData (TimeZone tz) => NFData (LocalDate cal tz) where

   rnf (LocalDate date zone) = rnf date `seq` rnf zone `seq` ()

instance NFData (TimeZone tz) => NFData (LocalDateTime cal tz) where

   rnf (LocalDateTime time zone) = rnf time `seq` rnf zone `seq` ()

instance NFData (TimeZone tz) => NFData (LocalDateTimeNanos cal tz) where

   rnf (LocalDateTimeNanos time zone) = rnf time `seq` rnf zone `seq` ()

instance Storable (LocalDate cal SomeOffset) where
   sizeOf  _ = 6
   alignment = sizeOf
   peekElemOff ptr n = do
       let off = 6 * n
       date <- peek . plusPtr ptr $ off
       zone <- peek . plusPtr ptr $ off + 4
       return $! LocalDate date zone
   pokeElemOff ptr n (LocalDate date zone) = do
       let off = 6 * n
       flip poke date . plusPtr ptr $ off
       flip poke zone . plusPtr ptr $ off + 4

instance Storable (LocalDateTime cal SomeOffset) where
   sizeOf  _ = 10
   alignment = sizeOf
   peekElemOff ptr n = do
       let off = 10 * n
       time <- peek . plusPtr ptr $ off
       zone <- peek . plusPtr ptr $ off + 8
       return $! LocalDateTime time zone
   pokeElemOff ptr n (LocalDateTime time zone) = do
       let off = 10 * n
       flip poke time . plusPtr ptr $ off
       flip poke zone . plusPtr ptr $ off + 8

instance Storable (LocalDateTimeNanos cal SomeOffset) where
   sizeOf  _ = 14
   alignment = sizeOf
   peekElemOff ptr n = do
       let off = 14 * n
       time <- peek . plusPtr ptr $ off
       zone <- peek . plusPtr ptr $ off + 12
       return $! LocalDateTimeNanos time zone
   pokeElemOff ptr n (LocalDateTimeNanos time zone) = do
       let off = 14 * n
       flip poke time . plusPtr ptr $ off
       flip poke zone . plusPtr ptr $ off + 12