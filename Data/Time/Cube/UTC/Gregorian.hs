{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall                       #-}
{-# OPTIONS -fno-warn-orphans           #-}
{-# OPTIONS -fno-warn-name-shadowing    #-}
{-# OPTIONS -fno-warn-type-defaults     #-}

-- |
-- Module      : Data.Time.Cube.UTC.Gregorian
-- License     : BSD3
-- Maintainer  : Enzo Haussecker
-- Stability   : Stable
-- Portability : Portable
--
-- Gregorian instances for UTC timestamps.
module Data.Time.Cube.UTC.Gregorian (

 -- ** Timestamps
       UTCDate(..)
     , UTCDateTime(..)
     , UTCDateTimeNanos(..)

 -- ** Create
     , createUTCDate
     , createUTCDateTime
     , createUTCDateTimeNanos

 -- ** Current
     , getCurrentUTCDate
     , getCurrentUTCDateTime
     , getCurrentUTCDateTimeNanos

 -- ** Parsing
     , parseUTCDate
     , parseUTCDateTime
     , parseUTCDateTimeNanos

 -- ** Calendar
     , Era(..)
     , Month(..)
     , DayOfWeek(..)

     ) where

import Control.Applicative ((<$>))
import Control.Lens.Setter (over)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Cube.Base
import Data.Time.Cube.City
import Data.Time.Cube.Format
import Data.Time.Cube.Lens
import Data.Time.Cube.Parser
import Data.Time.Cube.Unix.Gregorian
import Data.Time.Cube.Utilities
import Data.Time.Cube.UTC
import Data.Time.Cube.Zone
import System.Locale (TimeLocale)
import Text.Printf (printf)

deriving instance Bounded (UTCDate Gregorian)
deriving instance Enum    (UTCDate Gregorian)

instance Bounded (UTCDateTime Gregorian) where

    -- |
    -- 12:00:00 AM Thursday 01 January 1970 UTC.
    minBound = UTCDateTime 0

    -- |
    -- 11:59:59 PM Friday 31 December 9999 UTC.
    maxBound = UTCDateTime 253402257624

instance Bounded (UTCDateTimeNanos Gregorian) where

    -- |
    -- 12:00:00.000000000 AM Thursday 01 January 1970 UTC.
    minBound = UTCDateTimeNanos 0 0

    -- |
    -- 11:59:59.999999999 PM Friday 31 December 9999 UTC.
    maxBound = UTCDateTimeNanos 253402257624 999999999

instance Enum (UTCDateTime Gregorian) where

    -- |
    -- Next second.
    succ = flip plus $ Second 1

    -- |
    -- Previous second.
    pred = flip plus . Second $ - 1

    -- |
    -- Unenumerate a UTC timestamp.
    fromEnum (UTCDateTime base) = fromIntegral base

    -- |
    -- Enumerate a UTC timestamp.
    toEnum base = 
      if minBound <= time && time <= maxBound
      then time else error "toEnum{UTCDateTime Gregorian}: out of range" where
           time = UTCDateTime $ fromIntegral base

instance Human (UTCDate Gregorian) where

    -- |
    -- Define the Gregorian components of a UTC datestamp.
    type Components (UTCDate Gregorian) = DateStruct Gregorian

    -- |
    -- Pack a UTC datestamp from Gregorian components.
    pack DateStruct{..} = createUTCDate _d_year _d_mon _d_mday

    -- |
    -- Unpack a UTC datestamp into Gregorian components.
    unpack (UTCDate unix) = unpack unix

instance Human (UTCDateTime Gregorian) where

    -- |
    -- Define the Gregorian components of a UTC timestamp.
    type Components (UTCDateTime Gregorian) = DateTimeStruct Gregorian

    -- |
    -- Pack a UTC timestamp from Gregorian components.
    pack DateTimeStruct{..} =
      createUTCDateTime _dt_year _dt_mon _dt_mday _dt_hour _dt_min sec
      where sec = round _dt_sec :: Second

    -- |
    -- Unpack a UTC timestamp into Gregorian components.
    unpack (UTCDateTime base) =
      over dt_sec (+ leap) $ unpack (UnixDateTime unix :: UnixDateTime Gregorian)
      where (,) unix leap = baseUTCToUnix base

instance Human (UTCDateTimeNanos Gregorian) where

    -- |
    -- Define the Gregorian components of a UTC timestamp with nanosecond granularity.
    type Components (UTCDateTimeNanos Gregorian) = DateTimeStruct Gregorian

    -- |
    -- Pack a UTC timestamp with nanosecond granularity from Gregorian components.
    pack DateTimeStruct{..} =
      createUTCDateTimeNanos _dt_year _dt_mon _dt_mday _dt_hour _dt_min sec nsec
      where (,) sec nsec = properFracNanos _dt_sec

    -- |
    -- Unpack a UTC timestamp with nanosecond granularity into Gregorian components.
    unpack (UTCDateTimeNanos base nsec) =
      over dt_sec (+ leap) $ unpack (UnixDateTimeNanos unix nsec :: UnixDateTimeNanos Gregorian)
      where (,) unix leap = baseUTCToUnix base

instance Math (UTCDate Gregorian) Day where

    -- |
    -- Compute the day duration between two UTC datestamps.
    duration (UTCDate old) (UTCDate new) = duration old new

    -- |
    -- Add days to a UTC datestamp.
    plus (UTCDate unix) day = UTCDate $ plus unix day

instance Math (UTCDateTime Gregorian) Second where

    -- |
    -- Compute the second duration between two UTC timetamps.
    duration (UTCDateTime old) (UTCDateTime new) = Second (new - old)

    -- |
    -- Add seconds to a UTC timetamp.
    plus (UTCDateTime base) Second{..} =
      if minBound <= time && time <= maxBound
      then time else error "plus{UTCDateTime Gregorian, Second}: out of range" where
           time = UTCDateTime $ base + getSecond

instance Math (UTCDateTimeNanos Gregorian) Second where

    -- |
    -- Compute the second duration between two UTC timestamps with nanosecond granularity.
    duration (UTCDateTimeNanos old _) (UTCDateTimeNanos new _) = Second (new - old)

    -- |
    -- Add seconds to a UTC timestamp with nanosecond granularity.
    plus (UTCDateTimeNanos base nsec) Second{..} =
      if minBound <= time && time <= maxBound
      then time else error "plus{UTCDateTimeNanos Gregorian, Second}: out of range" where
           time = UTCDateTimeNanos (base + getSecond) nsec

instance Math (UTCDateTimeNanos Gregorian) Millis where

    -- |
    -- Compute the millisecond duration between two UTC timestamps with nanosecond granularity.
    duration old new = toMillis new - toMillis old
      where toMillis (UTCDateTimeNanos base nsec) =
              Millis $ base * 1000 + fromIntegral nsec `div` 1000000

    -- |
    -- Add milliseconds to a UTC timestamp with nanosecond granularity.
    plus (UTCDateTimeNanos base nsec) Millis{..} =
      if minBound <= time && time <= maxBound
      then time else error "plus{UTCDateTimeNanos Gregorian, Millis}: out of range" where
           time = UTCDateTimeNanos (base + extra) (fromIntegral nanos)
           (,) extra nanos = (fromIntegral nsec + getMillis * 1000000) `divMod` 1000000000

instance Math (UTCDateTimeNanos Gregorian) Micros where

    -- |
    -- Compute the microsecond duration between two UTC timestamps with nanosecond granularity.
    duration old new = toMicros new - toMicros old
      where toMicros (UTCDateTimeNanos base nsec) =
              Micros $ base * 1000000 + fromIntegral nsec `div` 1000

    -- |
    -- Add microseconds to a UTC timestamp with nanosecond granularity.
    plus (UTCDateTimeNanos base nsec) Micros{..} =
      if minBound <= time && time <= maxBound
      then time else error "plus{UTCDateTimeNanos Gregorian, Micros}: out of range" where
           time = UTCDateTimeNanos (base + extra) (fromIntegral nanos)
           (,) extra nanos = (fromIntegral nsec + getMicros * 1000) `divMod` 1000000000

instance Math (UTCDateTimeNanos Gregorian) Nanos where

    -- |
    -- Compute the nanosecond duration between two UTC timestamps with nanosecond granularity.
    duration old new =
      if result < toInteger (maxBound :: Int64) then fromInteger result
      else error "duration{UTCDateTimeNanos Gregorian, Nanos}: integer overflow" where
           result = toNanos new - toNanos old
           toNanos (UTCDateTimeNanos base nsec) =
             toInteger base * 1000000000 + toInteger nsec

    -- |
    -- Add nanoseconds to a UTC timestamp with nanosecond granularity.
    plus (UTCDateTimeNanos base nsec) Nanos{..} =
      if minBound <= time && time <= maxBound
      then time else error "plus{UTCDateTimeNanos Gregorian, Nanos}: out of range" where
           time = UTCDateTimeNanos (base + extra) (fromIntegral nanos)
           (,) extra nanos = (fromIntegral nsec + getNanos) `divMod` 1000000000

instance Show (UTCDate Gregorian) where
    show date = printf "%s %02d %s %4d UTC" wday _d_mday mon _d_year where
         mon  = show _d_mon
         wday = show _d_wday
         DateStruct{..} = unpack date

instance Show (UTCDateTime Gregorian) where
    show time = printf "%02d:%02d:%02d %s %s %02d %s %4d UTC" hour _dt_min sec ampm wday _dt_mday mon _dt_year where
         mon  = show  _dt_mon
         wday = show  _dt_wday
         sec  = round _dt_sec :: Second
         (,) ampm hour = getPeriod _dt_hour
         DateTimeStruct{..} = unpack time

instance Show (UTCDateTimeNanos Gregorian) where
    show time = printf "%02d:%02d:%02d.%09d %s %s %02d %s %4d UTC" hour _dt_min sec nsec ampm wday _dt_mday mon _dt_year where
         mon  = show _dt_mon
         wday = show _dt_wday
         (,) sec  nsec = properFracNanos _dt_sec
         (,) ampm hour = getPeriod _dt_hour
         DateTimeStruct{..} = unpack time

-- |
-- Create a UTC date.
createUTCDate :: Year -> Month Gregorian -> Day -> UTCDate Gregorian
createUTCDate year mon day = UTCDate $ createUnixDate year mon day

-- |
-- Create a UTC date and time.
createUTCDateTime :: Year -> Month Gregorian -> Day -> Hour -> Minute -> Second -> UTCDateTime Gregorian
createUTCDateTime year mon day hour min Second{..} =
   if minBound <= time && time <= maxBound
   then time else error "createUTCDateTime: out of range" where
        time = UTCDateTime base
        base = baseUnixToUTC unix + getSecond
        UnixDateTime unix = createUnixDateTime year mon day hour min 0

-- |
-- Create a UTC date and time with nanosecond granularity.
createUTCDateTimeNanos :: Year -> Month Gregorian -> Day -> Hour -> Minute -> Second -> Nanos -> UTCDateTimeNanos Gregorian
createUTCDateTimeNanos year mon day hour min Second{..} Nanos{..} =
   if minBound <= time && time <= maxBound
   then time else error "createUTCDateTimeNanos: out of range" where
        time = UTCDateTimeNanos base nsec
        base = baseUnixToUTC unix + getSecond + extra
        (,) extra nsec = fmap fromIntegral $ divMod getNanos 1000000000
        UnixDateTime unix = createUnixDateTime year mon day hour min 0

-- |
-- Get the current UTC date from the system clock.
getCurrentUTCDate :: IO (UTCDate Gregorian)
getCurrentUTCDate = UTCDate <$> getCurrentUnixDate

-- |
-- Get the current UTC date and time from the system clock.
getCurrentUTCDateTime :: IO (UTCDateTime Gregorian)
getCurrentUTCDateTime = do
  UnixDateTime unix <- getCurrentUnixDateTime
  return $! UTCDateTime $ baseUnixToUTC unix

-- |
-- Get the current UTC date and time with nanosecond granularity from the system clock.
getCurrentUTCDateTimeNanos :: IO (UTCDateTimeNanos Gregorian)
getCurrentUTCDateTimeNanos = do
  UnixDateTimeNanos unix nsec <- getCurrentUnixDateTimeNanos
  let base = baseUnixToUTC unix
      date = UTCDate . UnixDate . fromIntegral $ div unix 86400 
      time = UTCDateTimeNanos base nsec
      leap = Nanos . round $ realToFrac (mod unix 86400) * 11574.074074074073
  return $! if maybe True (/= date) nextLeap
    then time
    else time `plus` leap

-- |
-- Initialize the parser state.
state :: ParserState Gregorian
state =  ParserState 1970 January 1 Thursday 0 0 0.0 id id utc

-- |
-- Parse a UTC date.
parseUTCDate :: TimeLocale -> FormatText -> Text -> Either String (UTCDate Gregorian)
parseUTCDate locale format = fmap UTCDate . parseUnixDate locale format

-- |
-- Parse a UTC date and time.
parseUTCDateTime :: TimeLocale -> FormatText -> Text -> Either String (UTCDateTime Gregorian)
parseUTCDateTime locale format =
    fmap from . parse locale state Universal format
    where from ParserState{..} =
               createUTCDateTime _set_year _set_mon _set_mday hour _set_min sec
               where hour = _set_ampm _set_hour
                     sec  = truncate _set_sec

-- |
-- Parse a UTC date and time with nanosecond granularity.
parseUTCDateTimeNanos :: TimeLocale -> FormatText -> Text -> Either String (UTCDateTimeNanos Gregorian)
parseUTCDateTimeNanos locale format =
    fmap from . parse locale state Universal format
    where from ParserState{..} =
               createUTCDateTimeNanos _set_year _set_mon _set_mday hour _set_min sec nsec
               where hour = _set_ampm _set_hour
                     (,) sec nsec = properFracNanos $ _set_frac _set_sec

-- |
-- Next leap second insertion date.
nextLeap :: Maybe (UTCDate Gregorian)
nextLeap = Nothing
