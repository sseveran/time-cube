{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS -Wall               #-}

-- |
-- Module      : Data.Time.Cube.Local
-- Copyright   : Copyright (c) 2015, Alpha Heavy Industries, Inc. All rights reserved.
-- License     : BSD3
-- Maintainer  : Enzo Haussecker <ehaussecker@alphaheavy.com>
-- Stability   : Experimental
-- Portability : Portable
--
-- Local timestamps.
module Data.Time.Cube.Local (

 -- ** Local Timestamps
       LocalDate(..)
     , LocalDateTime(..)
     , LocalDateTimeNanos(..)

     ) where

import Control.DeepSeq      (NFData(..))
import Data.Ord             (comparing)
import Data.Time.Cube.Base  (Calendar(..))
import Data.Time.Cube.UTC
import Data.Time.Cube.Zones (SomeOffset, TimeZone)
import Foreign.Ptr          (plusPtr)
import Foreign.Storable     (Storable(..))
import GHC.Generics         (Generic)

-- |
-- Local days since Unix epoch.
data LocalDate (cal :: Calendar) geo =
     LocalDate {-# UNPACK #-} !(UTCDate cal) {-# UNPACK #-} !(TimeZone geo)
     deriving Generic

-- |
-- Local seconds since Unix epoch (including leap seconds).
data LocalDateTime (cal :: Calendar) geo =
     LocalDateTime {-# UNPACK #-} !(UTCDateTime cal) {-# UNPACK #-} !(TimeZone geo)
     deriving Generic

-- |
-- Local nanoseconds since Unix epoch (including leap seconds).
data LocalDateTimeNanos (cal :: Calendar) geo =
     LocalDateTimeNanos {-# UNPACK #-} !(UTCDateTimeNanos cal) {-# UNPACK #-} !(TimeZone geo)
     deriving Generic

deriving instance (Eq geo, Eq (TimeZone geo)) => Eq (LocalDate cal geo)
deriving instance (Eq geo, Eq (TimeZone geo)) => Eq (LocalDateTime cal geo)
deriving instance (Eq geo, Eq (TimeZone geo)) => Eq (LocalDateTimeNanos cal geo)

instance (Eq geo, Eq (TimeZone geo)) => Ord (LocalDate cal geo) where

   compare = comparing $ \ (LocalDate date _) -> date

instance (Eq geo, Eq (TimeZone geo)) => Ord (LocalDateTime cal geo) where

   compare = comparing $ \ (LocalDateTime time _) -> time

instance (Eq geo, Eq (TimeZone geo)) => Ord (LocalDateTimeNanos cal geo) where

   compare = comparing $ \ (LocalDateTimeNanos time _) -> time

instance NFData (TimeZone geo) => NFData (LocalDate cal geo) where

   rnf (LocalDate date zone) = rnf date `seq` rnf zone `seq` ()

instance NFData (TimeZone geo) => NFData (LocalDateTime cal geo) where

   rnf (LocalDateTime time zone) = rnf time `seq` rnf zone `seq` ()

instance NFData (TimeZone geo) => NFData (LocalDateTimeNanos cal geo) where

   rnf (LocalDateTimeNanos time zone) = rnf time `seq` rnf zone `seq` ()

instance Storable (LocalDate cal SomeOffset) where
   sizeOf = const 6
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
   sizeOf = const 10
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
   sizeOf = const 14
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