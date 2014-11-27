{-# LANGUAGE BangPatterns            #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts        #-}
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
-- Gregorian instances for Unix timestamps.
module Data.Time.Cube.Unix.Gregorian (

 -- ** Timestamps
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

 -- ** Parsing
     , parseUnixDate
     , parseUnixDateTime
     , parseUnixDateTimeNanos

 -- ** State
     , ParserState
     , defaultUnixParserState

 -- ** Internals
     , parseUnixDate'
     , parseUnixDateTime'
     , parseUnixDateTimeNanos'

 -- ** Calendar
     , Era(..)
     , Month(..)
     , DayOfWeek(..)

     ) where

import Control.Arrow ((***))
import Control.Lens.Setter (over)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Cube.Base
import Data.Time.Cube.Format
import Data.Time.Cube.Lens
import Data.Time.Cube.Parser
import Data.Time.Cube.Unix
import Data.Time.Cube.Utilities
import Data.Time.Cube.Zones
import Foreign.C.Types (CLong(..))
import Foreign.C.Time (C'timeval(..), getTimeOfDay)
import GHC.Generics (Generic)
import System.Locale (TimeLocale, defaultTimeLocale)
import Text.Printf (printf)

data instance Era Gregorian =
     BeforeChrist
   | AnnoDomini

deriving instance Bounded (Era Gregorian)
deriving instance Eq      (Era Gregorian)
deriving instance Enum    (Era Gregorian)
deriving instance Generic (Era Gregorian)
deriving instance Ord     (Era Gregorian)
deriving instance Read    (Era Gregorian)
deriving instance Show    (Era Gregorian)

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

deriving instance Bounded (Month Gregorian)
deriving instance Eq      (Month Gregorian)
deriving instance Generic (Month Gregorian)
deriving instance Ord     (Month Gregorian)
deriving instance Read    (Month Gregorian)
deriving instance Show    (Month Gregorian)

instance Enum (Month Gregorian) where
   fromEnum January   = 01
   fromEnum February  = 02
   fromEnum March     = 03
   fromEnum April     = 04
   fromEnum May       = 05
   fromEnum June      = 06
   fromEnum July      = 07
   fromEnum August    = 08
   fromEnum September = 09
   fromEnum October   = 10
   fromEnum November  = 11
   fromEnum December  = 12
   toEnum 01 = January
   toEnum 02 = February
   toEnum 03 = March
   toEnum 04 = April
   toEnum 05 = May
   toEnum 06 = June
   toEnum 07 = July
   toEnum 08 = August
   toEnum 09 = September
   toEnum 10 = October
   toEnum 11 = November
   toEnum 12 = December
   toEnum __ = error "toEnum{Month Gregorian}: out of range"

data instance DayOfWeek Gregorian =
     Sunday
   | Monday
   | Tuesday
   | Wednesday
   | Thursday
   | Friday
   | Saturday

deriving instance Bounded (DayOfWeek Gregorian)
deriving instance Eq      (DayOfWeek Gregorian)
deriving instance Generic (DayOfWeek Gregorian)
deriving instance Ord     (DayOfWeek Gregorian)
deriving instance Read    (DayOfWeek Gregorian)
deriving instance Show    (DayOfWeek Gregorian)

instance Enum (DayOfWeek Gregorian) where
   fromEnum Sunday    = 1
   fromEnum Monday    = 2
   fromEnum Tuesday   = 3
   fromEnum Wednesday = 4
   fromEnum Thursday  = 5
   fromEnum Friday    = 6
   fromEnum Saturday  = 7
   toEnum 1 = Sunday
   toEnum 2 = Monday
   toEnum 3 = Tuesday
   toEnum 4 = Wednesday
   toEnum 5 = Thursday
   toEnum 6 = Friday
   toEnum 7 = Saturday
   toEnum _ = error "toEnum{DayOfWeek Gregorian}: out of range"

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
    -- Pack a Unix datestamp from Gregorian components.
    pack DateStruct{..} = createUnixDate _d_year _d_mon _d_mday

    -- |
    -- Unpack a Unix datestamp into Gregorian components.
    unpack (UnixDate base) =
       rec 1970 $ Day base where
       rec !year !days =
           if days >= size
           then rec (year + 1) (days - size)
           else DateStruct year month mday wday
           where wday = toEnum $ 1 + (fromIntegral base + 4) `mod` 7
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
    -- Pack a Unix timestamp from Gregorian components.
    pack DateTimeStruct{..} =
      createUnixDateTime _dt_year _dt_mon _dt_mday _dt_hour _dt_min sec
      where sec = round _dt_sec :: Second

    -- |
    -- Unpack a Unix timestamp into Gregorian components.
    unpack (UnixDateTime base) = 
      DateTimeStruct _d_year _d_mon _d_mday _d_wday hour min sec
      where DateStruct{..} = unpack (UnixDate date :: UnixDate Gregorian)
            (,) date mod1  = fromIntegral *** Hour $ divMod base 86400
            (,) hour mod2  = fmap fromIntegral     $ divMod mod1 03600
            (,) min  sec   = fmap realToFrac       $ divMod mod2 00060

instance Human (UnixDateTimeNanos Gregorian) where

    -- |
    -- Define the Gregorian components of a Unix timestamp with nanosecond granularity.
    type Components (UnixDateTimeNanos Gregorian) = DateTimeStruct Gregorian

    -- |
    -- Pack a Unix timestamp with nanosecond granularity from Gregorian components.
    pack DateTimeStruct{..} =
      createUnixDateTimeNanos _dt_year _dt_mon _dt_mday _dt_hour _dt_min sec nsec
      where (,) sec nsec = properFracNanos _dt_sec

    -- |
    -- Unpack a Unix timestamp with nanosecond granularity into Gregorian components.
    unpack (UnixDateTimeNanos base nsec) =
      over dt_sec (+ frac) $ unpack time
      where time = UnixDateTime base :: UnixDateTime Gregorian
            frac = realToFrac nsec / 1000000000

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
           time = UnixDateTimeNanos (base + fromIntegral day * 86400) nsec

instance Math (UnixDateTimeNanos Gregorian) Hour where

    -- |
    -- Compute the hour duration between two Unix timestamps with nanosecond granularity.
    duration (UnixDateTimeNanos old _) (UnixDateTimeNanos new _) = Hour (new - old) `div` 3600

    -- |
    -- Add hours to a Unix timestamp with nanosecond granularity.
    plus (UnixDateTimeNanos base nsec) Hour{..} =
      if minBound <= time && time <= maxBound
      then time else error "plus{UnixDateTimeNanos Gregorian, Hour}: out of range" where
           time = UnixDateTimeNanos (base + getHour * 3600) nsec

instance Math (UnixDateTimeNanos Gregorian) Minute where

    -- |
    -- Compute the minute duration between two Unix timestamps with nanosecond granularity.
    duration (UnixDateTimeNanos old _) (UnixDateTimeNanos new _) = Minute (new - old) `div` 60

    -- |
    -- Add minutes to a Unix timestamp with nanosecond granularity.
    plus (UnixDateTimeNanos base nsec) Minute{..} =
      if minBound <= time && time <= maxBound
      then time else error "plus{UnixDateTimeNanos Gregorian, Minute}: out of range" where
           time = UnixDateTimeNanos (base + getMinute * 60) nsec

instance Math (UnixDateTimeNanos Gregorian) Second where

    -- |
    -- Compute the second duration between two Unix timestamps with nanosecond granularity.
    duration (UnixDateTimeNanos old _) (UnixDateTimeNanos new _) = Second (new - old)

    -- |
    -- Add seconds to a Unix timestamp with nanosecond granularity.
    plus (UnixDateTimeNanos base nsec) Second{..} =
      if minBound <= time && time <= maxBound
      then time else error "plus{UnixDateTimeNanos Gregorian, Second}: out of range" where
           time = UnixDateTimeNanos (base + getSecond) nsec

instance Math (UnixDateTimeNanos Gregorian) Millis where

    -- |
    -- Compute the millisecond duration between two Unix timestamps with nanosecond granularity.
    duration old new = toMillis new - toMillis old
      where toMillis (UnixDateTimeNanos base nsec) =
              Millis $ base * 1000 + fromIntegral nsec `div` 1000000

    -- |
    -- Add milliseconds to a Unix timestamp with nanosecond granularity.
    plus (UnixDateTimeNanos base nsec) Millis{..} =
      if minBound <= time && time <= maxBound
      then time else error "plus{UnixDateTimeNanos Gregorian, Millis}: out of range" where
           time = UnixDateTimeNanos (base + extra) (fromIntegral nanos)
           (,) extra nanos = (fromIntegral nsec + getMillis * 1000000) `divMod` 1000000000

instance Math (UnixDateTimeNanos Gregorian) Micros where

    -- |
    -- Compute the microsecond duration between two Unix timestamps with nanosecond granularity.
    duration old new = toMicros new - toMicros old
      where toMicros (UnixDateTimeNanos base nsec) =
              Micros $ base * 1000000 + fromIntegral nsec `div` 1000

    -- |
    -- Add microseconds to a Unix timestamp with nanosecond granularity.
    plus (UnixDateTimeNanos base nsec) Micros{..} =
      if minBound <= time && time <= maxBound
      then time else error "plus{UnixDateTimeNanos Gregorian, Micros}: out of range" where
           time = UnixDateTimeNanos (base + extra) (fromIntegral nanos)
           (,) extra nanos = (fromIntegral nsec + getMicros * 1000) `divMod` 1000000000

instance Math (UnixDateTimeNanos Gregorian) Nanos where

    -- |
    -- Compute the nanosecond duration between two Unix timestamps with nanosecond granularity.
    duration old new =
      if result < toInteger (maxBound :: Int64) then fromInteger result
      else error "duration{UnixDateTimeNanos Gregorian, Nanos}: integer overflow" where
           result = toNanos new - toNanos old
           toNanos (UnixDateTimeNanos base nsec) =
             toInteger base * 1000000000 + toInteger nsec

    -- |
    -- Add nanoseconds to a Unix timestamp with nanosecond granularity.
    plus (UnixDateTimeNanos base nsec) Nanos{..} =
      if minBound <= time && time <= maxBound
      then time else error "plus{UnixDateTimeNanos Gregorian, Nanos}: out of range" where
           time = UnixDateTimeNanos (base + extra) (fromIntegral nanos)
           (,) extra nanos = (fromIntegral nsec + getNanos) `divMod` 1000000000

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
         (,) ampm hour = getPeriod _dt_hour
         DateTimeStruct{..} = unpack time

instance Show (UnixDateTimeNanos Gregorian) where
    show time = printf "%02d:%02d:%02d.%09d %s %s %02d %s %4d" hour _dt_min sec nsec ampm wday _dt_mday mon _dt_year where
         mon  = show _dt_mon
         wday = show _dt_wday
         (,) sec  nsec = properFracNanos _dt_sec
         (,) ampm hour = getPeriod _dt_hour
         DateTimeStruct{..} = unpack time

-- |
-- Create a Unix date.
createUnixDate
  :: Year            -- ^ Year
  -> Month Gregorian -- ^ Month
  -> Day             -- ^ Day
  -> UnixDate Gregorian
createUnixDate year mon day =
  if minBound <= date && date <= maxBound
  then date else error "createUnixDate: out of range" where
       date = UnixDate . getDay $ unsafeEpochToDate year mon day

-- |
-- Create a Unix date and time.
createUnixDateTime
  :: Year            -- ^ Year
  -> Month Gregorian -- ^ Month
  -> Day             -- ^ Day
  -> Hour            -- ^ Hour
  -> Minute          -- ^ Minute
  -> Second          -- ^ Second
  -> UnixDateTime Gregorian
createUnixDateTime year mon day hour min sec =
  if minBound <= time && time <= maxBound
  then time else error "createUnixDateTime: out of range" where
       time = UnixDateTime . getSecond $ unsafeEpochToTime year mon day hour min sec

-- |
-- Create a Unix date and time with nanosecond granularity.
createUnixDateTimeNanos
  :: Year            -- ^ Year
  -> Month Gregorian -- ^ Month
  -> Day             -- ^ Day
  -> Hour            -- ^ Hour
  -> Minute          -- ^ Minute
  -> Second          -- ^ Second
  -> Nanos           -- ^ Nanosecond
  -> UnixDateTimeNanos Gregorian
createUnixDateTimeNanos year mon day hour min sec Nanos{..} =
  if minBound <= time && time <= maxBound
  then time else error "createUnixDateTimeNanos: out of range" where
       time = UnixDateTimeNanos (base + extra) nsec
       Second base = unsafeEpochToTime year mon day hour min sec
       (,) extra nsec = fmap fromIntegral $ divMod getNanos 1000000000

-- |
-- Get the current Unix date from the system clock.
getCurrentUnixDate :: IO (UnixDate Gregorian)
getCurrentUnixDate =
  getTimeOfDay >>= \ (C'timeval (CLong base) _) ->
  return $! UnixDate . fromIntegral $ base `div` 86400

-- |
-- Get the current Unix date and time from the system clock.
getCurrentUnixDateTime :: IO (UnixDateTime Gregorian)
getCurrentUnixDateTime =
  getTimeOfDay >>= \ (C'timeval (CLong base) _) ->
  return $! UnixDateTime base

-- |
-- Get the current Unix date and time with nanosecond granularity from the system clock.
getCurrentUnixDateTimeNanos :: IO (UnixDateTimeNanos Gregorian)
getCurrentUnixDateTimeNanos =
  getTimeOfDay >>= \ (C'timeval (CLong base) (CLong usec)) ->
  return $! UnixDateTimeNanos base $ fromIntegral usec * 1000

-- |
-- Default parser state for unix timestamps.
defaultUnixParserState :: ParserState Gregorian Universal
defaultUnixParserState =  ParserState 1970 January 1 Thursday 0 0 0.0 id id CoordinatedUniversalTime

-- |
-- Parse a Unix date.
parseUnixDate
  :: FormatText -- ^ Format string
  -> Text       -- ^ Input string
  -> Either String (UnixDate Gregorian)
parseUnixDate = parseUnixDate' defaultTimeLocale defaultUnixParserState

-- |
-- Parse a Unix date and time.
parseUnixDateTime
  :: FormatText -- ^ Format string
  -> Text       -- ^ Input string
  -> Either String (UnixDateTime Gregorian)
parseUnixDateTime = parseUnixDateTime' defaultTimeLocale defaultUnixParserState

-- |
-- Parse a Unix date and time with nanosecond granularity.
parseUnixDateTimeNanos
  :: FormatText -- ^ Format string
  -> Text       -- ^ Input string
  -> Either String (UnixDateTimeNanos Gregorian)
parseUnixDateTimeNanos = parseUnixDateTimeNanos' defaultTimeLocale defaultUnixParserState

-- |
-- Same as 'parseUnixDate', except takes the
-- additional locale and parser state parameters. 
parseUnixDate'
  :: Abbreviate (TimeZone tz)
  => TimeLocale               -- ^ Local conventions
  -> ParserState Gregorian tz -- ^ Initialized state
  -> FormatText               -- ^ Format string
  -> Text                     -- ^ Input string
  -> Either String (UnixDate Gregorian)
parseUnixDate' locale state format =
  fmap from . parse locale state format
  where from ParserState{..} =
             createUnixDate _ps_year _ps_mon _ps_mday

-- |
-- Same as 'parseUnixDateTime', except takes the
-- additional locale and parser state parameters. 
parseUnixDateTime'
  :: Abbreviate (TimeZone tz)
  => TimeLocale               -- ^ Local conventions
  -> ParserState Gregorian tz -- ^ Initialized state
  -> FormatText               -- ^ Format string
  -> Text                     -- ^ Input string
  -> Either String (UnixDateTime Gregorian)
parseUnixDateTime' locale state format =
  fmap from . parse locale state format
  where from ParserState{..} =
             createUnixDateTime _ps_year _ps_mon _ps_mday hour _ps_min sec
             where hour = _ps_ampm _ps_hour
                   sec  = truncate _ps_sec

-- |
-- Same as 'parseUnixDateTimeNanos', except takes
-- the additional locale and parser state parameters.
parseUnixDateTimeNanos'
  :: Abbreviate (TimeZone tz)
  => TimeLocale               -- ^ Local conventions
  -> ParserState Gregorian tz -- ^ Initialized state
  -> FormatText               -- ^ Format string
  -> Text                     -- ^ Input string
  -> Either String (UnixDateTimeNanos Gregorian)
parseUnixDateTimeNanos' locale state format =
  fmap from . parse locale state format
  where from ParserState{..} =
             createUnixDateTimeNanos _ps_year _ps_mon _ps_mday hour _ps_min sec nsec
             where hour = _ps_ampm _ps_hour
                   (,) sec nsec = properFracNanos $ _ps_frac _ps_sec

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
