{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall                  #-}
{-# OPTIONS -fno-warn-orphans      #-}

-- |
-- Module      : Data.Time.Cube.Unix.Gregorian
-- Copyright   : Copyright (c) 2014, Alpha Heavy Industries, Inc.
-- License     : BSD3
-- Maintainer  : Enzo Haussecker <enzo@ucsd.edu>
-- Stability   : Stable
-- Portability : Portable
--
-- A Gregorian-based implementation of Unix timestamps.
module Data.Time.Cube.Unix.Gregorian {-(

 -- ** Type
       UnixDate(..)

 -- ** Create
     , createUnixDate

 -- ** Current
     , getCurrentUnixDate

 -- ** Utilities
     , isLeapYear
     , unsafeEpochToDate

     )-} where

import Data.Time.Cube.Base
import Data.Time.Cube.Unix
import Data.Time.Cube.Zone
import Foreign.C.Time (C'timeval(..), getTimeOfDay)
import GHC.Generics (Generic)
import Text.Printf (printf)

data instance Era Gregorian =
     BeforeChrist
   | AnnoDomini

data instance Month Gregorian =
     January
   | February
   | March
   | April
   | May
   | June
   | July
   | August
   | September
   | October
   | November
   | December

data instance DayOfWeek Gregorian =
     Sunday
   | Monday
   | Tuesday
   | Wednesday
   | Thursday
   | Friday
   | Saturday

deriving instance Eq (Era Gregorian)
deriving instance Eq (Month Gregorian)
deriving instance Eq (DayOfWeek Gregorian)

deriving instance Enum (Era Gregorian)
deriving instance Enum (Month Gregorian)
deriving instance Enum (DayOfWeek Gregorian)

deriving instance Generic (Era Gregorian)
deriving instance Generic (Month Gregorian)
deriving instance Generic (DayOfWeek Gregorian)

deriving instance Ord (Era Gregorian)
deriving instance Ord (Month Gregorian)
deriving instance Ord (DayOfWeek Gregorian)

deriving instance Read (Era Gregorian)
deriving instance Read (Month Gregorian)
deriving instance Read (DayOfWeek Gregorian)

deriving instance Show (Era Gregorian)
deriving instance Show (Month Gregorian)
deriving instance Show (DayOfWeek Gregorian)

instance Bounded (UnixDate Gregorian tz) where

    -- |
    -- Minimum bound, 1970-01-01.
    minBound = UnixDate 0

    -- |
    -- Maximum bound, 9999-12-31.
    maxBound = UnixDate 2932896

instance Enum (UnixDate Gregorian tz) where

    succ = flip plus $ Day 1
    pred = flip plus . Day $ - 1

    fromEnum = fromIntegral . getDate
    toEnum x = 
      if minBound <= date && date <= maxBound
      then date else error "toEnum{UnixDate Gregorian tz}: date out of range" where
           date = UnixDate $ fromIntegral x

instance Human (UnixDate Gregorian tz) where

    -- |
    -- Define the Gregorian components of a Unix date.
    type Components (UnixDate Gregorian tz) = DateStruct Gregorian tz

    -- |
    -- Compose a Unix date from Gregorian components.
    pack DateStruct{..} = createUnixDate _d_year _d_mon _d_mday

    -- |
    -- Decompose a Unix date into Gregorian components.
    unpack UnixDate{..} =
        go 1970 $ Day getDate where
        go !year !days =
           if days >= size
           then go (year + 1) (days - size)
           else DateStruct year month mday wday
           where wday = toEnum $ (fromIntegral getDate + 4) `mod` 7
                 leap = isLeapYear year
                 size = if leap then 366 else 365
                 (,) month mday =
                     if leap
                     then if days >= 182
                          then if days >= 274
                               then if days >= 335
                                    then (December, days - 334)
                                    else if days >= 305
                                         then (November, days - 304)
                                         else (October , days - 273)
                               else if days >= 244
                                    then (September, days - 243)
                                    else if days >= 213
                                         then (August, days - 212)
                                         else (July  , days - 181)
                          else if days >= 091
                               then if days >= 152
                                    then (June, days - 151)
                                    else if days >= 121
                                         then (May  , days - 120)
                                         else (April, days - 090)
                               else if days >= 060
                                    then (March, days - 059)
                                    else if days >= 031
                                         then (February, days - 030)
                                         else (January , days + 001)
                     else if days >= 181
                          then if days >= 273
                               then if days >= 334
                                    then (December, days - 333)
                                    else if days >= 304
                                         then (November, days - 303)
                                         else (October , days - 272)
                               else if days >= 243
                                    then (September, days - 242)
                                    else if days >= 212
                                         then (August, days - 211)
                                         else (July  , days - 180)
                          else if days >= 090
                               then if days >= 151
                                    then (June, days - 150)
                                    else if days >= 120
                                         then (May  , days - 119)
                                         else (April, days - 089)
                               else if days >= 059
                                    then (March, days - 058)
                                    else if days >= 031
                                         then (February, days - 030)
                                         else (January , days + 001)

instance Math (UnixDate Gregorian tz) Day where -- FIXME: test for different TZs

    -- |
    -- Compute the day duration between two Unix dates.
    duration (UnixDate old) (UnixDate new) = Day (new - old)

    -- |
    -- Add days to a Unix date.
    plus UnixDate{..} Day{..} =
      if minBound <= date && date <= maxBound
      then date else error "plus{UnixDate Gregorian, Day}: date out of range" where
           date = UnixDate $ getDate + getDay

instance Show (UnixDate Gregorian CoordinatedUniversalTime) where
    show date = printf "%04d-%02d-%02d UTC" _d_year mon _d_mday where
         mon  = fromEnum _d_mon + 1
         DateStruct{..} = unpack date

instance Show (UnixDate Gregorian PacificDaylightTime) where
    show date = printf "%04d-%02d-%02d PDT" _d_year mon _d_mday where
         mon  = fromEnum _d_mon + 1
         DateStruct{..} = unpack date

instance Show (UnixDate Gregorian PacificStandardTime) where
    show date = printf "%04d-%02d-%02d PST" _d_year mon _d_mday where
         mon  = fromEnum _d_mon + 1
         DateStruct{..} = unpack date

instance Show (UnixDate Gregorian Unspecified) where
    show date = printf "%04d-%02d-%02d" _d_year mon _d_mday where
         mon  = fromEnum _d_mon + 1
         DateStruct{..} = unpack date

-- |
-- Create a Unix date.
createUnixDate :: Year -> Month Gregorian -> Day -> UnixDate Gregorian tz
createUnixDate year month day =
  if minBound <= date && date <= maxBound
  then date else error "createUnixDate: date out of range" where
       date = UnixDate . getDay $ unsafeEpochToDate year month day

-- |
-- Get the current Unix date from the system clock.
getCurrentUnixDate :: IO (UnixDate Gregorian Unspecified)
getCurrentUnixDate =
   getTimeOfDay >>= \ (C'timeval base _) ->
   return $! UnixDate . fromIntegral $ base `div` 86400

-- |
-- Check if the given year is a leap year.
isLeapYear :: Year -> Bool
isLeapYear year = year `mod` 400 == 0 || (year `mod` 100 /= 0 && year `mod` 4 == 0)

-- |
-- Calculate the number of days that have elapsed
-- between Unix epoch and the given Gregorian date
-- without performing any bounds check.
unsafeEpochToDate :: Year -> Month Gregorian -> Day -> Day
unsafeEpochToDate year month day =
  unsafeEpochToYear year + yearToMonth month leap + day - 1
  where leap = isLeapYear year

-- |
-- Calculate the number of days that have elapsed
-- between Unix epoch and the given year without
-- performing any bounds check.
unsafeEpochToYear :: Year -> Day
unsafeEpochToYear (Year year) =
  Day $ (year - 1970)   *   365 + (year - 1969) `div` 004 -
        (year - 1901) `div` 100 + (year - 1601) `div` 400

-- |
-- Calculate the number of days that have elapsed
-- between January 1st and the given Gregorian month.
yearToMonth :: Month Gregorian -> Bool -> Day
yearToMonth month leap =
  if leap
  then case month of
       January   -> 000; February -> 031; March    -> 060; April    -> 091
       May       -> 121; June     -> 152; July     -> 182; August   -> 213
       September -> 244; October  -> 274; November -> 305; December -> 335
  else case month of
       January   -> 000; February -> 031; March    -> 059; April    -> 090
       May       -> 120; June     -> 151; July     -> 181; August   -> 212
       September -> 243; October  -> 273; November -> 304; December -> 334
