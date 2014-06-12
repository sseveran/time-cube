{-# OPTIONS -Wall #-}

-- |
-- Module      : Data.Time.Cube.Parser
-- License     : BSD3
-- Maintainer  : Enzo Haussecker
-- Stability   : Stable
-- Portability : Portable
--
-- A generic timestamp parser.
module Data.Time.Cube.Parser where

import Data.Text (Text)

-- |
-- The format string is composed of various %-codes, each
-- representing time-related information described below.
--
-- [@%%@] A literal '%' character.
-- [@%A@] The full weekday name according to the current locale.
-- [@%a@] The abbreviated weekday name according to the current locale.
-- [@%B@] The full month name according to the current locale.
-- [@%b@] The abbreviated month name according to the current locale.
-- [@%D@] Equivalent to %m\/%d\/%y.
-- [@%d@] The day of the month as a decimal number (range 01 to 31).
-- [@%e@] Like %d, the day of the month as a decimal number, but a leading zero is replaced by a space.
-- [@%F@] Equivalent to %Y-%m-%d (the ISO 8601 date format).
-- [@%H@] The hour as a decimal number using a 24-hour clock (range 00 to 23).
-- [@%h@] Equivalent to %b.
-- [@%I@] The hour as a decimal number using a 12-hour clock (range 01 to 12).
-- [@%l@] Like %I, the hour as a decimal number using a 12-hour clock, but a leading zero is replaced by a space.
-- [@%M@] The minute as a decimal number (range 00 to 59).
-- [@%m@] The month as a decimal number (range 01 to 12).
-- [@%P@] Like %p, the period of the day according to the current locale, but lowercase.
-- [@%p@] The period of the day according to the current locale.
-- [@%Q@] The fraction of the second as a decimal number (range 0 to 999999999999).
-- [@%R@] Equivalent to %H:%M.
-- [@%r@] Equivalent to %I:%M:%S %p.
-- [@%S@] The second as a decimal number (range 00 to 60).
-- [@%T@] Equivalent to %H:%M:%S.
-- [@%Y@] The year as a decimal number (range 1970 to 9999).
-- [@%y@] The year as a decimal number without a century (range 00 to 99).
-- [@%Z@] The timezone abbreviation.
type FormatText = Text
