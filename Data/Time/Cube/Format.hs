{-# OPTIONS -Wall #-}

-- |
-- Module      : Data.Time.Cube.Format
-- License     : BSD3
-- Maintainer  : Enzo Haussecker
-- Stability   : Stable
-- Portability : Portable
--
-- Date and time format strings.
module Data.Time.Cube.Format (

 -- ** Format
       FormatText

     ) where

import Data.Text (Text)

-- |
-- The format string is composed of various %-codes, each
-- representing time-related information described below.
--
-- [@%%@] '%' literal.
-- [@%A@] Full weekday name according to the current locale.
-- [@%a@] Abbreviated weekday name according to the current locale.
-- [@%B@] Full month name according to the current locale.
-- [@%b@] Abbreviated month name according to the current locale.
-- [@%D@] Equivalent to %m\/%d\/%y.
-- [@%d@] Day of the month (01..31).
-- [@%e@] Like %d, the day of the month, but a leading zero is replaced with a space.
-- [@%F@] Equivalent to %Y-%m-%d.
-- [@%H@] Hour of the day using the 24-hour clock (00..23).
-- [@%h@] Equivalent to %b.
-- [@%I@] Hour of the day using the 12-hour clock (01..12).
-- [@%l@] Like %I, the hour of the day using the 12-hour clock, but a leading zero is replaced with a space.
-- [@%M@] Minute of the hour (00..59).
-- [@%m@] Month of the year (01..12).
-- [@%P@] Like %p, the period of the day according to the current locale, but lowercase.
-- [@%p@] Period of the day according to the current locale.
-- [@%Q@] Fraction of the second prefixed by a period (0..999999999).
-- [@%R@] Equivalent to %H:%M.
-- [@%r@] Equivalent to %I:%M:%S %p.
-- [@%S@] Second of the minute (00..60).
-- [@%T@] Equivalent to %H:%M:%S.
-- [@%Y@] Year of the era (1970..9999).
-- [@%y@] Year of the era without the century (00..99).
-- [@%Z@] Timezone abbreviation.
-- [@%z@] Numeric timezone.
type FormatText = Text
