{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall                       #-}

-- |
-- Module      : Data.Time.Cube.Base
-- Copyright   : Copyright (c) 2014, Alpha Heavy Industries, Inc. All rights reserved.
-- License     : Apache License, Version 2.0
-- Maintainer  : Enzo Haussecker <enzo@ucsd.edu>
-- Stability   : Experimental
-- Portability : Untested
--
-- Basic definitions, including data families, types, and functions.
--
module Data.Time.Cube.Base (

 -- ** Chronologies
       Calendar(..)
     , Era
     , Epoch(..)

 -- ** Components
     , Year(..)
     , Month
     , Day(..)
     , DayOfWeek
     , Hour(..)
     , Minute(..)
     , Second(..)
     , Millis(..)
     , Micros(..)
     , Nanos(..)
     , Picos(..)

 -- ** Fractions
     , properFracMillis
     , properFracMicros
     , properFracNanos
     , properFracPicos

     ) where

import Data.Int (Int32, Int64)
import GHC.Generics (Generic)

-- | System for organizing dates.
data Calendar =
     Gregorian
   | Hebrew
   | Islamic
   | Japanese
   | Julian
   deriving (Eq, Enum, Generic, Ord, Read, Show)

-- | System for numbering years.
data family Era (cal :: Calendar) :: *

-- | System origin.
data Epoch =
     Midnight
   | UnixEpoch
   deriving (Eq, Enum, Generic, Ord, Read, Show)

-- | Year.
newtype Year = Year Int32
   deriving (Bounded, Enum, Eq, Generic, Integral, Num, Ord, Real, Show)

-- | Month.
data family Month (cal :: Calendar) :: *

-- | Day.
newtype Day = Day Int32
   deriving (Bounded, Enum, Eq, Generic, Integral, Num, Ord, Real, Show)

-- | Day of week.
data family DayOfWeek (cal :: Calendar) :: *

-- | Hour.
newtype Hour = Hour Int64
   deriving (Bounded, Enum, Eq, Generic, Integral, Num, Ord, Real, Show)

-- | Minute.
newtype Minute = Minute Int64
   deriving (Bounded, Enum, Eq, Generic, Integral, Num, Ord, Real, Show)

-- | Second.
newtype Second = Second Int64
   deriving (Bounded, Enum, Eq, Generic, Integral, Num, Ord, Real, Show)

-- | Millisecond.
newtype Millis = Millis Int64
   deriving (Bounded, Enum, Eq, Generic, Integral, Num, Ord, Real, Show)

-- | Microsecond.
newtype Micros = Micros Int64
   deriving (Bounded, Enum, Eq, Generic, Integral, Num, Ord, Real, Show)

-- | Nanosecond.
newtype Nanos = Nanos Int64
   deriving (Bounded, Enum, Eq, Generic, Integral, Num, Ord, Real, Show)

-- | Picosecond.
newtype Picos = Picos Int64
   deriving (Bounded, Enum, Eq, Generic, Integral, Num, Ord, Real, Show)

-- | Decompose a floating point number into second and millisecond components.
properFracMillis :: RealFrac a => a -> (Second, Millis)
properFracMillis frac = if millis == 1000 then (sec + 1, 0) else res
   where res@(sec, millis) = fmap (round . (*) 1000) $ properFraction frac

-- | Decompose a floating point number into second and microsecond components.
properFracMicros :: RealFrac a => a -> (Second, Micros)
properFracMicros frac = if micros == 1000000 then (sec + 1, 0) else res
   where res@(sec, micros) = fmap (round . (*) 1000000) $ properFraction frac

-- | Decompose a floating point number into second and nanosecond components.
properFracNanos :: RealFrac a => a -> (Second, Nanos)
properFracNanos frac = if nanos == 1000000000 then (sec + 1, 0) else res
   where res@(sec, nanos) = fmap (round . (*) 1000000000) $ properFraction frac

-- | Decompose a floating point number into second and picosecond components.
properFracPicos :: RealFrac a => a -> (Second, Picos)
properFracPicos frac = if picos == 1000000000000 then (sec + 1, 0) else res
   where res@(sec, picos) = fmap (round . (*) 1000000000000) $ properFraction frac 
