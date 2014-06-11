{-# LANGUAGE BangPatterns            #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeFamilies            #-}
{-# OPTIONS -Wall                    #-}
{-# OPTIONS -fno-warn-orphans        #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

-- |
-- Module      : Data.Time.Cube.Unix.Gregorian
-- License     : BSD3
-- Maintainer  : Enzo Haussecker
-- Stability   : Stable
-- Portability : Portable
--
-- Gregorian-based Unix date and timestamps.
module Data.Time.Cube.Unix.Gregorian (

 -- ** Types
       UnixDate(..)
     , UnixDateTime(..)
     , UnixDateTimeNanos(..)

 -- ** Create
     , createUnixDate
     , createUnixDateTime
     , createUnixDateTimeNanos

 -- ** Current
     , getCurrentUnixDate
     , getCurrentUnixDateTime
     , getCurrentUnixDateTimeNanos

     ) where

import Control.Arrow ((***), second)
import Data.Time.Cube.Base
import Data.Time.Cube.Unix
import Foreign.C.Types (CLong(..))
import Foreign.C.Time (C'timeval(..), getTimeOfDay)
import GHC.Generics (Generic)
import Text.Printf (printf)

data instance Era Gregorian =
     BeforeChrist
   | AnnoDomini

deriving instance Eq (Era Gregorian)
deriving instance Enum (Era Gregorian)
deriving instance Generic (Era Gregorian)
deriving instance Ord (Era Gregorian)
deriving instance Read (Era Gregorian)
deriving instance Show (Era Gregorian)

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

deriving instance Eq (Month Gregorian)
deriving instance Enum (Month Gregorian)
deriving instance Generic (Month Gregorian)
deriving instance Ord (Month Gregorian)
deriving instance Read (Month Gregorian)
deriving instance Show (Month Gregorian)

data instance DayOfWeek Gregorian =
     Sunday
   | Monday
   | Tuesday
   | Wednesday
   | Thursday
   | Friday
   | Saturday

deriving instance Eq (DayOfWeek Gregorian)
deriving instance Enum (DayOfWeek Gregorian)
deriving instance Generic (DayOfWeek Gregorian)
deriving instance Ord (DayOfWeek Gregorian)
deriving instance Read (DayOfWeek Gregorian)
deriving instance Show (DayOfWeek Gregorian)

instance Bounded (UnixDate Gregorian) where

    -- |
    -- Thursday 01 January 1970.
    minBound = UnixDate 0

    -- |
    -- Friday 31 December 9999.
    maxBound = UnixDate 2932896

instance Bounded (UnixDateTime Gregorian) where

    -- |
    -- 12:00:00 AM Thursday 01 January 1970.
    minBound = UnixDateTime 0

    -- |
    -- 11:59:59 PM Friday 31 December 9999.
    maxBound = UnixDateTime 253402300799

instance Bounded (UnixDateTimeNanos Gregorian) where

    -- |
    -- 12:00:00.000000000 AM Thursday 01 January 1970.
    minBound = UnixDateTimeNanos 0 0

    -- |
    -- 11:59:59.999999999 PM Friday 31 December 9999.
    maxBound = UnixDateTimeNanos 253402300799 999999999

instance Enum (UnixDate Gregorian) where

    -- |
    -- Next day.
    succ = flip plus $ Day 1

    -- |
    -- Previous day.
    pred = flip plus . Day $ - 1

    -- |
    -- Unenumerate a Unix datestamp.
    fromEnum (UnixDate base) = fromIntegral base

    -- |
    -- Enumerate a Unix datestamp.
    toEnum base = 
      if minBound <= date && date <= maxBound
      then date else error "toEnum{UnixDate Gregorian}: out of range" where
           date = UnixDate $ fromIntegral base

instance Enum (UnixDateTime Gregorian) where

    -- |
    -- Next second.
    succ = flip plus $ Second 1

    -- |
    -- Previous second.
    pred = flip plus . Second $ - 1

    -- |
    -- Unenumerate a Unix timestamp.
    fromEnum (UnixDateTime base) = fromIntegral base

    -- |
    -- Enumerate a Unix timestamp.
    toEnum base = 
      if minBound <= time && time <= maxBound
      then time else error "toEnum{UnixDateTime Gregorian}: out of range" where
           time = UnixDateTime $ fromIntegral base

instance Human (UnixDate Gregorian) where

    -- |
    -- Define the Gregorian components of a Unix datestamp.
    type Components (UnixDate Gregorian) = DateStruct Gregorian

    -- |
    -- Compose a Unix datestamp from Gregorian components.
    pack DateStruct{..} = createUnixDate _d_year _d_mon _d_mday

    -- |
    -- Decompose a Unix datestamp into Gregorian components.
    unpack (UnixDate base) =
       rec 1970 $ Day base where
       rec !year !days =
           if days >= size
           then rec (year + 1) (days - size)
           else DateStruct year month mday wday
           where wday = toEnum $ (fromIntegral base + 4) `mod` 7
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

instance Human (UnixDateTime Gregorian) where

    -- |
    -- Define the Gregorian components of a Unix timestamp.
    type Components (UnixDateTime Gregorian) = DateTimeStruct Gregorian

    -- |
    -- Compose a Unix timestamp from Gregorian components.
    pack DateTimeStruct{..} =
      createUnixDateTime _dt_year _dt_mon _dt_mday _dt_hour _dt_min sec
      where sec = round _dt_sec :: Second

    -- |
    -- Decompose a Unix timestamp into Gregorian components.
    unpack (UnixDateTime base) = 
      DateTimeStruct _d_year _d_mon _d_mday _d_wday hour min sec
      where DateStruct{..} = unpack (UnixDate date :: UnixDate Gregorian)
            (,) date mod1  = fromIntegral *** Hour $ divMod base 86400
            (,) hour mod2  = second fromIntegral   $ divMod mod1 03600
            (,) min  sec   = second realToFrac     $ divMod mod2 00060

instance Human (UnixDateTimeNanos Gregorian) where

    -- |
    -- Define the Gregorian components of a Unix timestamp with nanosecond granularity.
    type Components (UnixDateTimeNanos Gregorian) = DateTimeStruct Gregorian

    -- |
    -- Compose a Unix timestamp with nanosecond granularity from Gregorian components.
    pack DateTimeStruct{..} =
      createUnixDateTimeNanos _dt_year _dt_mon _dt_mday _dt_hour _dt_min sec nsec
      where (,) sec nsec = properFracNanos _dt_sec

    -- |
    -- Decompose a Unix timestamp with nanosecond granularity into Gregorian components.
    unpack (UnixDateTimeNanos base nsec) =
      struct{_dt_sec = sec + realToFrac nsec / 1000000000}
      where time = UnixDateTime base :: UnixDateTime Gregorian
            struct@DateTimeStruct{_dt_sec = sec} = unpack time

instance Math (UnixDate Gregorian) Day where

    -- |
    -- Compute the day duration between two Unix datestamps.
    duration (UnixDate old) (UnixDate new) = Day (new - old)

    -- |
    -- Add days to a Unix datestamp.
    plus (UnixDate base) Day{..} =
      if minBound <= date && date <= maxBound
      then date else error "plus{UnixDate Gregorian, Day}: out of range" where
           date = UnixDate $ base + getDay

instance Math (UnixDateTime Gregorian) Day where

    -- |
    -- Compute the day duration between two Unix timestamps.
    duration (UnixDateTime old) (UnixDateTime new) = fromIntegral $ (new - old) `div` 86400

    -- |
    -- Add days to a Unix timestamp.
    plus (UnixDateTime base) day =
      if minBound <= time && time <= maxBound
      then time else error "plus{UnixDateTime Gregorian, Day}: out of range" where
           time = UnixDateTime $ base + fromIntegral day * 86400

instance Math (UnixDateTime Gregorian) Hour where

    -- |
    -- Compute the hour duration between two Unix timestamps.
    duration (UnixDateTime old) (UnixDateTime new) = Hour (new - old) `div` 3600

    -- |
    -- Add hours to a Unix timestamp.
    plus (UnixDateTime base) Hour{..} =
      if minBound <= time && time <= maxBound
      then time else error "plus{UnixDateTime Gregorian, Hour}: out of range" where
           time = UnixDateTime $ base + getHour * 3600

instance Math (UnixDateTime Gregorian) Minute where

    -- |
    -- Compute the minute duration between two Unix timestamps.
    duration (UnixDateTime old) (UnixDateTime new) = Minute (new - old) `div` 60

    -- |
    -- Add minutes to a Unix timestamp.
    plus (UnixDateTime base) Minute{..} =
      if minBound <= time && time <= maxBound
      then time else error "plus{UnixDateTime Gregorian, Minute}: out of range" where
           time = UnixDateTime $ base + getMinute * 60

instance Math (UnixDateTime Gregorian) Second where

    -- |
    -- Compute the second duration between two Unix timestamps.
    duration (UnixDateTime old) (UnixDateTime new) = Second (new - old)

    -- |
    -- Add seconds to a Unix timestamp.
    plus (UnixDateTime base) Second{..} =
      if minBound <= time && time <= maxBound
      then time else error "plus{UnixDateTime Gregorian, Second}: out of range" where
           time = UnixDateTime $ base + getSecond

instance Math (UnixDateTimeNanos Gregorian) Day where

    -- |
    -- Compute the day duration between two Unix timestamps with nanosecond granularity.
    duration (UnixDateTimeNanos old _) (UnixDateTimeNanos new _) = fromIntegral $ (new - old) `div` 86400

    -- |
    -- Add days to a Unix timestamp with nanosecond granularity.
    plus (UnixDateTimeNanos base nsec) day =
      if minBound <= time && time <= maxBound
      then time else error "plus{UnixDateTimeNanos Gregorian, Day}: out of range" where
           time = flip UnixDateTimeNanos nsec $ base + fromIntegral day * 86400

instance Show (UnixDate Gregorian) where
    show date = printf "%s %02d %s %4d" wday _d_mday mon _d_year where
         mon  = show _d_mon
         wday = show _d_wday
         DateStruct{..} = unpack date

instance Show (UnixDateTime Gregorian) where
    show time = printf "%02d:%02d:%02d %s %s %02d %s %4d" hour _dt_min sec ampm wday _dt_mday mon _dt_year where
         mon  = show  _dt_mon
         wday = show  _dt_wday
         sec  = round _dt_sec :: Second
         (,) ampm hour = unsafePeriod _dt_hour
         DateTimeStruct{..} = unpack time

instance Show (UnixDateTimeNanos Gregorian) where
    show time = printf "%02d:%02d:%02d.%09d %s %s %02d %s %4d" hour _dt_min sec nsec ampm wday _dt_mday mon _dt_year where
         mon  = show _dt_mon
         wday = show _dt_wday
         (,) sec  nsec = properFracNanos _dt_sec
         (,) ampm hour = unsafePeriod _dt_hour
         DateTimeStruct{..} = unpack time

-- |
-- Create a Unix datestamp.
createUnixDate :: Year -> Month Gregorian -> Day -> UnixDate Gregorian
createUnixDate year mon day =
  if minBound <= date && date <= maxBound
  then date else error "createUnixDate: out of range" where
       date = UnixDate . getDay $ unsafeEpochToDate year mon day

-- |
-- Create a Unix timestamp.
createUnixDateTime :: Year -> Month Gregorian -> Day -> Hour -> Minute -> Second -> UnixDateTime Gregorian
createUnixDateTime year mon day hour min sec =
  if minBound <= time && time <= maxBound
  then time else error "createUnixDateTime: out of range" where
       time = UnixDateTime . getSecond $ unsafeEpochToTime year mon day hour min sec

-- |
-- Create a Unix timestamp with nanosecond granularity.
createUnixDateTimeNanos :: Year -> Month Gregorian -> Day -> Hour -> Minute -> Second -> Nanos -> UnixDateTimeNanos Gregorian
createUnixDateTimeNanos year mon day hour min sec Nanos{..} =
  if minBound <= time && time <= maxBound
  then time else error "createUnixDateTimeNanos: out of range" where
       time = UnixDateTimeNanos (base + over) nsec
       Second base = unsafeEpochToTime year mon day hour min sec
       (,) over nsec = fmap fromIntegral $ divMod getNanos 1000000000

-- |
-- Get the current Unix datestamp from the system clock.
getCurrentUnixDate :: IO (UnixDate Gregorian)
getCurrentUnixDate =
   getTimeOfDay >>= \ (C'timeval (CLong base) _) ->
   return $! UnixDate . fromIntegral $ base `div` 86400

-- |
-- Get the current Unix timestamp from the system clock.
getCurrentUnixDateTime :: IO (UnixDateTime Gregorian)
getCurrentUnixDateTime =
   getTimeOfDay >>= \ (C'timeval (CLong base) _) ->
   return $! UnixDateTime base

-- |
-- Get the current Unix timestamp with nanosecond granularity from the system clock.
getCurrentUnixDateTimeNanos :: IO (UnixDateTimeNanos Gregorian)
getCurrentUnixDateTimeNanos =
   getTimeOfDay >>= \ (C'timeval (CLong base) (CLong usec)) ->
   return $! UnixDateTimeNanos base $ fromIntegral usec * 1000

-- |
-- Check if the given year is a leap year.
isLeapYear :: Year -> Bool
isLeapYear year = year `mod` 400 == 0 || (year `mod` 100 /= 0 && year `mod` 4 == 0)

-- |
-- Calculate the number of seconds that have elapsed
-- between Unix epoch and the given Gregorian date
-- and time without performing any bounds check.
unsafeEpochToTime :: Year -> Month Gregorian -> Day -> Hour -> Minute -> Second -> Second
unsafeEpochToTime year mon day Hour{..} Minute{..} Second{..} =
  Second $ fromIntegral getDay * 86400 + getHour * 3600 + getMinute * 60 + getSecond
  where Day{..} = unsafeEpochToDate year mon day

-- |
-- Calculate the number of days that have elapsed
-- between Unix epoch and the given Gregorian date
-- without performing any bounds check.
unsafeEpochToDate :: Year -> Month Gregorian -> Day -> Day
unsafeEpochToDate year mon day =
  unsafeEpochToYear year + yearToMonth mon leap + day - 1
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
yearToMonth mon leap =
  if leap
  then case mon of
       January   -> 000; February -> 031; March    -> 060; April    -> 091
       May       -> 121; June     -> 152; July     -> 182; August   -> 213
       September -> 244; October  -> 274; November -> 305; December -> 335
  else case mon of
       January   -> 000; February -> 031; March    -> 059; April    -> 090
       May       -> 120; June     -> 151; July     -> 181; August   -> 212
       September -> 243; October  -> 273; November -> 304; December -> 334

-- |
-- Show the 12-hour pariod (ante or post meridiem) of the
-- given 24-hour hour without performing any bounds check.
unsafePeriod :: Hour -> (String, Hour)
unsafePeriod hour
  | hour == 00 = ("AM", 12)
  | hour <= 11 = ("AM", hour)
  | hour == 12 = ("PM", hour)
  | otherwise  = ("PM", hour - 12)
