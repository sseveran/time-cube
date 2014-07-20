{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wall              #-}

-- |
-- Module      : Data.Time.Cube.Local.Gregorian
-- License     : BSD3
-- Maintainer  : Enzo Haussecker
-- Stability   : Stable
-- Portability : Portable
--
-- Gregorian instances for local timestamps.
module Data.Time.Cube.Local.Gregorian (

 -- ** Timestamps
       LocalDate(..)
     , LocalDateTime(..)
     , LocalDateTimeNanos(..)

 -- ** Calendar
     , Era(..)
     , Month(..)
     , DayOfWeek(..)

     ) where

import Data.Time.Cube.Base
import Data.Time.Cube.City
import Data.Time.Cube.Format
import Data.Time.Cube.Lens
import Data.Time.Cube.Local
import Data.Time.Cube.Parser
import Data.Time.Cube.UTC.Gregorian
import Data.Time.Cube.Zone

instance Bounded (LocalDate Gregorian) where

    -- |
    -- 12:00:00 AM Thursday 01 January 1970 UTC.
    minBound = LocalDate minBound utc

    -- |
    -- 11:59:59 PM Friday 31 December 9999 UTC.
    maxBound = LocalDate maxBound utc
