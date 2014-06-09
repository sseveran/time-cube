{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall                  #-}
{-# OPTIONS -fno-warn-orphans      #-}

-- |
-- Module      : Data.Time.Cube.Unix.Date.Gregorian
-- Copyright   : Copyright (c) 2014, Alpha Heavy Industries, Inc. All rights reserved.
-- License     : Apache License, Version 2.0
-- Maintainer  : Enzo Haussecker <enzo@ucsd.edu>
-- Stability   : Stable
-- Portability : Portable
--
-- Gregorian instances and utilities for Unix datestamps.
module Data.Time.Cube.Unix.Date.Gregorian (

 -- ** Create
       createUnixDate

 -- ** Utilities
     , isLeapYear
     , unsafeEpochToDate

     ) where

import Data.Time.Cube.Base (Calendar(Gregorian), Year(..), Day(..))
import Data.Time.Cube.Calendar.Gregorian (Month(..))
import Data.Time.Cube.Human (Human(..))
import Data.Time.Cube.Math (Math(..))
import Data.Time.Cube.Struct (DateStruct(..))
import Data.Time.Cube.Unix.Date (UnixDate(..))
import Text.Printf (printf)

instance Bounded (UnixDate 'Gregorian) where
    minBound = UnixDate 0
    maxBound = UnixDate 2932896

instance Enum (UnixDate 'Gregorian) where
    succ = flip plus $ Day 1
    pred = flip plus . Day $ - 1
    fromEnum = fromIntegral . getBase
    toEnum x = 
        if minBound <= date && date <= maxBound
        then date else error "toEnum{UnixDate 'Gregorian}: date out of range" where
             date = UnixDate $ fromIntegral x

instance Human (UnixDate 'Gregorian) where

    -- |
    -- Define the Gregorian components of a Unix date.
    type Components (UnixDate 'Gregorian) = DateStruct 'Gregorian

    -- |
    -- Compose a Unix date from Gregorian components.
    pack DateStruct{..} = createUnixDate _d_year _d_mon _d_mday

    -- |
    -- Decompose a Unix date into Gregorian components.
    unpack UnixDate{..} =
        go 1970 $ Day getBase where
        go !year !days =
           if days >= size
           then go (year + 1) (days - size)
           else DateStruct year month mday wday
           where wday = toEnum $ (fromIntegral getBase + 4) `mod` 7
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

instance Math (UnixDate 'Gregorian) Day where
    plus UnixDate{..} Day{..} =
      if minBound <= date && date <= maxBound
      then date else error "plus{UnixDate 'Gregorian, Day}: date out of range" where
           date = UnixDate $ getBase + getDay

instance Show (UnixDate 'Gregorian) where
    show date = printf "%04d-%02d-%02d" _d_year mon _d_mday where
         mon  = fromEnum _d_mon + 1
         DateStruct{..} = unpack date

-- |
-- Create a Unix date.
--
-- > >>> createUnixDate 2013 November 03
-- > 2013-11-03
--
createUnixDate :: Year -> Month 'Gregorian -> Day -> UnixDate 'Gregorian
createUnixDate year month day =
  if minBound <= date && date <= maxBound
  then date else error "createUnixDate: date out of range" where
       date = UnixDate . getDay $ unsafeEpochToDate year month day

-- |
-- Check if the given year is a leap year.
isLeapYear :: Year -> Bool
isLeapYear year = year `mod` 400 == 0 || (year `mod` 100 /= 0 && year `mod` 4 == 0)

-- |
-- Calculate the number of days that have elapsed
-- between Unix epoch and the given Gregorian date
-- without performing any bounds check.
unsafeEpochToDate :: Year -> Month 'Gregorian -> Day -> Day
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
yearToMonth :: Month 'Gregorian -> Bool -> Day
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
