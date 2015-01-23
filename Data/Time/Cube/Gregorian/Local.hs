{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE TypeFamilies            #-}
{-# OPTIONS -Wall                    #-}
{-# OPTIONS -fno-warn-orphans        #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

-- |
-- Module      : Data.Time.Cube.Local.Gregorian
-- License     : BSD3
-- Maintainer  : Enzo Haussecker
-- Stability   : Stable
-- Portability : Portable
--
-- Gregorian instances for local timestamps.
module Data.Time.Cube.Gregorian.Local (

 -- ** Local Timestamps
       LocalDate(..)
     , LocalDateTime(..)
     , LocalDateTimeNanos(..)

     , createLocalDate
 --  , createLocalDateTime
 --  , createLocalDateTimeNanos

     ) where

import Data.Time.Cube.Base
import Data.Time.Cube.Gregorian.UTC
import Data.Time.Cube.Gregorian.Unix
import Data.Time.Cube.Local
import Data.Time.Cube.Parser
import Data.Time.Cube.Zone

import qualified Text.Printf as P (printf)
import qualified Data.Text   as T (unpack)

instance Bounded (LocalDate Gregorian UTC) where

    -- |
    -- Thursday 01 January 1970 UTC.
    minBound = LocalDate minBound UTC

    -- |
    -- Friday 31 December 9999 UTC.
    maxBound = LocalDate maxBound UTC

instance Bounded (LocalDateTime Gregorian UTC) where

    -- |
    -- 12:00:00 AM Thursday 01 January 1970 UTC.
    minBound = LocalDateTime minBound UTC

    -- |
    -- 11:59:59 PM Friday 31 December 9999 UTC.
    maxBound = LocalDateTime maxBound UTC

instance Bounded (LocalDateTimeNanos Gregorian UTC) where

    -- |
    -- 12:00:00.000000000 AM Thursday 01 January 1970 UTC.
    minBound = LocalDateTimeNanos minBound UTC

    -- |
    -- 11:59:59.999999999 PM Friday 31 December 9999 UTC.
    maxBound = LocalDateTimeNanos maxBound UTC

instance Human (LocalDate Gregorian tz) where

    -- |
    -- Define the Gregorian components of a local datestamp.
    type Components (LocalDate Gregorian tz) = LocalDateStruct Gregorian tz

    -- |
    -- Pack a local datestamp from Gregorian components.
    pack LocalDateStruct{..} =
      createLocalDate _ld_year _ld_mon _ld_mday _ld_zone

    -- |
    -- Unpack a local datestamp into Gregorian components.
    unpack (LocalDate date zone) =
      LocalDateStruct _d_year _d_mon _d_mday _d_wday zone
      where DateStruct{..} = unpack date

instance Math (LocalDate Gregorian tz) Day where

    -- |
    -- Compute the day duration between two local datestamps.
    duration (LocalDate old _) (LocalDate new _) = duration old new

    -- |
    -- Add days to a local datestamp.
    plus (LocalDate date zone) day = LocalDate (date `plus` day) zone

instance Abbreviate (TimeZone tz) => Show (LocalDate Gregorian tz) where
    show date = P.printf "%s %02d %s %4d %s" wday _ld_mday mon _ld_year abbr where
         mon  = show _ld_mon
         wday = show _ld_wday
         abbr = T.unpack $ abbreviate _ld_zone
         LocalDateStruct{..} = unpack date

-- |
-- Create a local date.
createLocalDate
  :: Year            -- Year
  -> Month Gregorian -- Month
  -> Day             -- Day
  -> TimeZone tz    -- Time zone
  -> LocalDate Gregorian tz
createLocalDate year mon day =
  LocalDate $ createUTCDate year mon day

-- |
-- Default parser state.
defaultLocalParserState :: ParserState Gregorian UTC
defaultLocalParserState =  ParserState 1970 January 1 Thursday 0 0 0.0 id id UTC









{-
import Data.Time.Cube.City
import Data.Time.Cube.Format
import Data.Time.Cube.Lens
import Data.Time.Cube.Utilities









instance Human (LocalDateTime Gregorian) where

    -- |
    -- Define the Gregorian components of a local timestamp.
    type Components (LocalDateTime Gregorian) = LocalDateTimeStruct Gregorian

    -- |
    -- Pack a local timestamp from Gregorian components.
    pack LocalDateTimeStruct{..} =
      createLocalDateTime _ldt_year _ldt_mon _ldt_mday _ldt_hour _ldt_min sec _ldt_zone
      where sec = round _ldt_sec :: Second

    -- |
    -- Unpack a local timestamp into Gregorian components.
    unpack (LocalDateTime (UTCDateTime base) zone) =
      LocalDateTimeStruct _dt_year _dt_mon _dt_mday _dt_wday _dt_hour _dt_min sec zone
      where DateTimeStruct{..} = unpack $ time `plus` offs
            (,) unix leap = baseUTCToUnix base
            time = UnixDateTime unix :: UnixDateTime Gregorian
            offs = fromIntegral $ getUTCOffset zone :: Minute
            sec  = _dt_sec + leap

instance Human (LocalDateTimeNanos Gregorian) where

    -- |
    -- Define the Gregorian components of a local timestamp with nanosecond granularity.
    type Components (LocalDateTimeNanos Gregorian) = LocalDateTimeStruct Gregorian

    -- |
    -- Pack a local timestamp with nanosecond granularity from Gregorian components.
    pack LocalDateTimeStruct{..} =
      createLocalDateTimeNanos _ldt_year _ldt_mon _ldt_mday _ldt_hour _ldt_min sec nsec _ldt_zone
      where (,) sec nsec = properFracNanos _ldt_sec

    -- |
    -- Unpack a local timestamp with nanosecond granularity into Gregorian components.
    unpack (LocalDateTimeNanos (UTCDateTimeNanos base nsec) zone) =
      LocalDateTimeStruct _dt_year _dt_mon _dt_mday _dt_wday _dt_hour _dt_min sec zone
      where DateTimeStruct{..} = unpack $ time `plus` offs
            (,) unix leap = baseUTCToUnix base
            time = UnixDateTimeNanos unix nsec :: UnixDateTimeNanos Gregorian
            offs = fromIntegral $ getUTCOffset zone :: Minute
            sec  = _dt_sec + leap



instance Show (LocalDateTime Gregorian) where
    show time = printf "%02d:%02d:%02d %s %s %02d %s %4d %s" hour _ldt_min sec ampm wday _ldt_mday mon _ldt_year abbr where
         mon  = show _ldt_mon
         wday = show _ldt_wday
         abbr = show $ abbreviate _ldt_zone
         sec  = round _ldt_sec :: Second
         (,) ampm hour = getPeriod _ldt_hour
         LocalDateTimeStruct{..} = unpack time

instance Show (LocalDateTimeNanos Gregorian) where
    show time = printf "%02d:%02d:%02d.%09d %s %s %02d %s %4d %s" hour _ldt_min sec nsec ampm wday _ldt_mday mon _ldt_year abbr where
         mon  = show _ldt_mon
         wday = show _ldt_wday
         abbr = show $ abbreviate _ldt_zone
         (,) sec  nsec = properFracNanos _ldt_sec
         (,) ampm hour = getPeriod _ldt_hour
         LocalDateTimeStruct{..} = unpack time


-- |
-- Create a local date and time.
createLocalDateTime :: Year -> Month Gregorian -> Day -> Hour -> Minute -> Second -> TimeZone -> LocalDateTime Gregorian
createLocalDateTime year mon day hour min Second{..} zone =
   if minBound <= time && time <= maxBound
   then time else error "createLocalDateTime: out of range" where
        time = LocalDateTime (UTCDateTime base) zone
        base = baseUnixToUTC (unix - offs) + getSecond
        offs = fromIntegral (getUTCOffset zone) * 60
        UnixDateTime unix = createUnixDateTime year mon day hour min 0

-- |
-- Parse a local date and time with nanosecond granularity.
createLocalDateTimeNanos :: Year -> Month Gregorian -> Day -> Hour -> Minute -> Second -> Nanos -> TimeZone -> LocalDateTimeNanos Gregorian
createLocalDateTimeNanos year mon day hour min Second{..} Nanos{..} zone =
   if minBound <= time && time <= maxBound
   then time else error "createLocalDateTimeNanos: out of range" where
        time = LocalDateTimeNanos (UTCDateTimeNanos base nsec) zone
        base = baseUnixToUTC (unix - offs) + getSecond + extra
        offs = fromIntegral (getUTCOffset zone) * 60
        UnixDateTime unix = createUnixDateTime year mon day hour min 0
        (,) extra nsec = fmap fromIntegral $ divMod getNanos 1000000000












-- | Get the current local date and time from the system clock.
--
-- > >>> getCurrentLocalDateTime New_York 
-- > 2013-11-03 16:38:16 EST
--
getCurrentLocalDateTime :: TZ.City -> IO LocalDateTime
getCurrentLocalDateTime city = getTransitions city >>= getCurrentLocalDateTime'

-- | Get the current local date and time from the system clock using preloaded transition
--   times.
--
-- > >>> ttimes <- getTransitions Moscow
-- > >>> getCurrentLocalDateTime' ttimes
-- > 2013-11-04 01:41:50 MSK
--
--   Use this function if you need to get the current local date and time more than once.
--   The use of preloaded transition times will avoid unnecessary parsing of Olson files.
getCurrentLocalDateTime' :: [Transition] -> IO LocalDateTime
getCurrentLocalDateTime' ttimes = do
   time@UnixDateTime{..} <- getCurrentUnixDateTime
   let  base = baseUnixToUTC _udt_sec_base
        f tt = localBase tt > base
        mval = listToMaybe $ dropWhile f ttimes
        zone = maybe (tz2w8 TZ.utc) localZone mval
   if   maybe True (/= convert time) nextLeap
   then return $! LocalDateTime base zone
   else let leap = round (realToFrac (_udt_sec_base `mod` 86400) / 86400 :: Double)
        in  return $! LocalDateTime base zone `plus` Second leap

-}




