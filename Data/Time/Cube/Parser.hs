{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS -Wall              #-}

-- |
-- Module      : Data.Time.Cube.Parser
-- License     : BSD3
-- Maintainer  : Enzo Haussecker
-- Stability   : Stable
-- Portability : Portable
--
-- Generic timestamp parsing.
module Data.Time.Cube.Parser where

import Control.Applicative ((<|>), (<$>), (*>))
import Control.Arrow ((***))
import Control.Lens.Setter (ASetter, assign)
import Control.Lens.TH (makeLenses)
import Control.Monad ((<=<))
import Control.Monad.State.Strict (execState, State)
import Data.Attoparsec.Text as P
import Data.Char (isAlpha)
import Data.Text as T (Text, length, pack, unpack)
import Data.Time.Cube.Base
import Data.Time.Cube.City
import Data.Time.Cube.Zone
import System.Locale (TimeLocale(..))

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

-- |
-- Generic timestamp parser state.
data ParserState (cal :: Calendar) = ParserState {
    _set_year :: Year
  , _set_mon  :: Month cal
  , _set_mday :: Day
  , _set_wday :: DayOfWeek cal
  , _set_hour :: Hour
  , _set_min  :: Minute
  , _set_sec  :: Double
  , _set_frac :: Double -> Double
  , _set_ampm :: Hour   -> Hour
  , _set_zone :: TimeZone
  }

makeLenses ''ParserState

-- |
-- and return the raw state 

parse
  :: Read (DayOfWeek cal)
  => Read (Month cal)
  => TimeLocale
  -> ParserState cal
  -> City
  -> FormatText
  -> Text
  -> Either String (ParserState cal)
parse locale state city format text = do
  flip parseOnly text <=< fmap execute $ parseOnly parser format
  where parser = many' $ createFormatParser locale city
        execute setters = flip execState state <$> sequence <$> sequence setters

-- | Create a format text parser.
createFormatParser
  :: Read (DayOfWeek cal)
  => Read (Month cal)
  => TimeLocale
  -> City
  -> Parser (Parser (State (ParserState cal) ()))
createFormatParser locale city =
      matchLit "%%"
  <|> matchSet "%A" set_wday (weekFull locale)
  <|> matchSet "%a" set_wday (weekAbbr locale)
  <|> matchSet "%B" set_mon  (monthFull locale)
  <|> matchSet "%b" set_mon  (monthAbbr locale)

  <|> matchSet "%Z" set_zone (timezone city)
  <|> matchTxt







-- |
-- Match a percent literal.
matchLit
  :: Text
  -> Parser (Parser (State (ParserState cal) ()))
matchLit code =
  string code *> return (char '%' *> return (return ()))

-- |
-- Match a percent code and assign the value returned by the parser.
matchSet
  :: Read (DayOfWeek cal)
  => Read (Month cal)
  => Text
  -> ASetter (ParserState cal) (ParserState cal) a a
  -> Parser a
  -> Parser (Parser (State (ParserState cal) ()))
matchSet code field parser = 
  string code *> return (assign field <$> parser)

-- | Match any other character sequence.
matchTxt
  :: Parser (Parser (State (ParserState cal) ()))
matchTxt = takeWhile1 (/='%') >>= return . \ source -> do
  target <- P.take $ T.length source
  if source == target then return (return ())
  else fail "matchTxt: mismatch"















-- |
-- Parse a time zone.
timezone :: City -> Parser TimeZone
timezone city =
  takeWhile1 isAlpha >>=
  either fail return . unabbreviate . TimeZoneAbbr city

-- |
-- Parse a month in short text format.
monthAbbr :: Read (Month cal) => TimeLocale -> Parser (Month cal)
monthAbbr = fromList . map (\(full, abbr) -> (T.pack abbr, read full)) . months

-- |
-- Parse a month in long text format.
monthFull :: Read (Month cal) => TimeLocale -> Parser (Month cal)
monthFull = fromList . map (\(full, _) -> (T.pack full, read full)) . months

-- |
-- Parse a day of week in short text format.
weekAbbr :: Read (DayOfWeek cal) => TimeLocale -> Parser (DayOfWeek cal)
weekAbbr = fromList . map (\(full, abbr) -> (T.pack abbr, read full)) . wDays

-- |
-- Parse a day of week in long text format. 
weekFull :: Read (DayOfWeek cal) => TimeLocale -> Parser (DayOfWeek cal)
weekFull = fromList . map (\(full, _) -> (T.pack full, read full)) . wDays

-- |
-- Create a parser from a list of key-value pairs.
fromList :: [(Text, a)] -> Parser a
fromList = foldl1 (<|>) . map (uncurry (*>) . (string *** return))
