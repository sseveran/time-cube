{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# OPTIONS -Wall                    #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

-- |
-- Module      : Data.Time.Cube.Parser
-- License     : BSD3
-- Maintainer  : Enzo Haussecker
-- Stability   : Stable
-- Portability : Portable
--
-- Timestamp parsing.
module Data.Time.Cube.Parser (

 -- ** Types
       FormatText
     , ParserState(..)

 -- ** Parsing
     , parse

     ) where

import Control.Applicative ((<|>), (<$>), (*>))
import Control.Arrow ((***))
import Control.Lens.Setter (Setter, (%=), assign)
import Control.Lens.TH (makeLenses)
import Control.Monad ((<=<), foldM, replicateM)
import Control.Monad.State.Strict (execState, State)
import Data.Attoparsec.Text as P hiding (parse)
import Data.Char (isAlpha)
import Data.Text as T (Text, length, pack, toLower)
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
-- Parser state.
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
-- Parse a timestamp and return the raw parser
-- state or an error string if the parser failed.
parse
  :: Bounded (Month cal)
  => Enum (Month cal)
  => Read (DayOfWeek cal)
  => Read (Month cal)
  => TimeLocale      -- ^ Local conventions
  -> ParserState cal -- ^ Initialized State
  -> City            -- ^ Reference location
  -> FormatText      -- ^ Format string
  -> Text            -- ^ Input string
  -> Either String (ParserState cal)
parse locale state city format input =
  flip parseOnly input <=< fmap execute . flip parseOnly format . many' $ formatParser locale city
  where execute setters = flip execState state <$> sequence <$> sequence setters

-- |
-- Format parser.
formatParser
   :: Bounded (Month cal)
   => Enum (Month cal)
   => Read (DayOfWeek cal)
   => Read (Month cal)
   => TimeLocale
   -> City
   -> Parser (Parser (State (ParserState cal) ()))
formatParser locale city =

  --- Literals...
      percent

  --- Components...
  <|> match "%A" set_wday (weekFull locale)
  <|> match "%B" set_mon (monthFull locale)
  <|> match "%H" set_hour (fixed 2)
  <|> match "%I" set_hour (fixed 2)
  <|> match "%M" set_min (fixed 2)
  <|> match "%P" set_ampm (period locale toLower)
  <|> match "%Q" set_frac fraction
  <|> match "%S" set_sec second
  <|> match "%Y" set_year (fixed 4)
  <|> match "%Z" set_zone (timezone city)
  <|> match "%a" set_wday (weekAbbr locale)
  <|> match "%b" set_mon (monthAbbr locale)
  <|> match "%d" set_mday (fixed 2)
  <|> match "%e" set_mday padded
  <|> match "%h" set_mon (monthAbbr locale)
  <|> match "%l" set_hour padded
  <|> match "%m" set_mon month
  <|> match "%p" set_ampm (period locale id)
  <|> match "%y" set_year year
  <|> match "%z" set_zone offset

  --- Combinators...
  <|> date     "%D" set_year set_mon set_mday
  <|> iso8601  "%F" set_year set_mon set_mday
  <|> clock12  "%r" set_hour set_min set_sec locale
  <|> clock24  "%T" set_hour set_min set_sec
  <|> clock24' "%R" set_hour set_min

  --- Text...
  <|> text

-- |
-- Match a percent literal.
percent :: Parser (Parser (State (ParserState cal) ()))
percent = string "%%" *> return (char '%' *> return (return ()))

-- |
-- Match a percent code and assign
-- the value returned by the parser.
match
  :: Read (DayOfWeek cal)
  => Read (Month cal)
  => Text
  -> Setter (ParserState cal) (ParserState cal) a a
  -> Parser a
  -> Parser (Parser (State (ParserState cal) ()))
match code field parser =
    string code *> return (assign field <$> parser)

-- |
-- Match an ISO 8601 date percent code and
-- assign the values returned by the parser.
iso8601
  :: Bounded (Month cal)
  => Enum (Month cal)
  => Text
  -> Setter (ParserState cal) (ParserState cal) Year Year
  -> Setter (ParserState cal) (ParserState cal) (Month cal) (Month cal)
  -> Setter (ParserState cal) (ParserState cal) Day Day
  -> Parser (Parser (State (ParserState cal) ()))
iso8601 code yr mon mday =
    string code *> return parser
    where parser = do
            y <- fixed 4; _ <- char '-'
            m <- month  ; _ <- char '-'
            d <- fixed 2
            return $! assign yr   y *>
                      assign mon  m *>
                      assign mday d

-- |
-- Match an American date percent code and
-- assign the values returned by the parser.
date
  :: Bounded (Month cal)
  => Enum (Month cal)
  => Text
  -> Setter (ParserState cal) (ParserState cal) Year Year
  -> Setter (ParserState cal) (ParserState cal) (Month cal) (Month cal)
  -> Setter (ParserState cal) (ParserState cal) Day Day
  -> Parser (Parser (State (ParserState cal) ()))
date code yr mon mday =
     string code *> return parser
     where parser = do
             m <- month  ; _ <- char '/'
             d <- fixed 2; _ <- char '/'
             y <- year
             return $! assign yr   y *>
                       assign mon  m *>
                       assign mday d

-- |
-- Match a 12-hour clock percent code and
-- assign the values returned by the parser.
clock12
  :: Text
  -> Setter (ParserState cal) (ParserState cal) Hour Hour
  -> Setter (ParserState cal) (ParserState cal) Minute Minute
  -> Setter (ParserState cal) (ParserState cal) Double Double
  -> TimeLocale
  -> Parser (Parser (State (ParserState cal) ()))
clock12 code hour min sec locale =
    string code *> return parser
    where parser = do
            h <- fixed 2; _ <- char ':'
            m <- fixed 2; _ <- char ':'
            s <- second ; _ <- char ' '
            p <- period locale id
            return $! assign hour h *>
                      assign min  m *>
                      assign sec  s *> (hour %= p)

-- |
-- Match a 24-hour clock percent code and
-- assign the values returned by the parser.
clock24
  :: Text
  -> Setter (ParserState cal) (ParserState cal) Hour Hour
  -> Setter (ParserState cal) (ParserState cal) Minute Minute
  -> Setter (ParserState cal) (ParserState cal) Double Double
  -> Parser (Parser (State (ParserState cal) ()))
clock24 code hour min sec =
    string code *> return parser
    where parser = do
            h <- fixed 2; _ <- char ':'
            m <- fixed 2; _ <- char ':'
            s <- second
            return $! assign hour h *>
                      assign min  m *>
                      assign sec  s

-- |
-- Same as 'clock24', but with the seconds omitted.
clock24'
  :: Text
  -> Setter (ParserState cal) (ParserState cal) Hour Hour
  -> Setter (ParserState cal) (ParserState cal) Minute Minute
  -> Parser (Parser (State (ParserState cal) ()))
clock24' code hour min =
    string code *> return parser
    where parser = do
            h <- fixed 2; _ <- char ':'
            m <- fixed 2
            return $! assign hour h *>
                      assign min  m

-- |
-- Match any other character sequence.
text :: Parser (Parser (State (ParserState cal) ()))
text = takeWhile1 (/='%') >>= return . \ source -> do
  target <- P.take $ T.length source
  if source == target then return (return ())
  else fail "text: mismatch"

-- |
-- Parse an integral type of fixed length.
fixed :: Read a => Integral a => Int -> Parser a
fixed n = fromInteger . read <$> replicateM n digit

-- |
-- Parse an integral type of two digits
-- or one digit preceded by a space.
padded :: Read a => Integral a => Parser a
padded = fixed 2 <|> (char ' ' *> fixed 1)

-- |
-- Parse a year.
year :: Parser Year
year = fixed 2 >>= \ n -> return $! n + if n < 70 then 2000 else 1900

-- |
-- Parse a month.
month :: forall cal . Bounded (Month cal) => Enum (Month cal) => Parser (Month cal)
month = do
   n <- fixed 2
   if   fromEnum (minBound :: Month cal) <= n &&
        fromEnum (maxBound :: Month cal) >= n
   then return $! toEnum n
   else fail "month: out of bounds"

-- |
-- Parse a second.
second :: Parser Double
second = (realToFrac :: Int -> Double) <$> fixed 2

-- |
-- Parse a fraction.
fraction :: Parser (Double -> Double)
fraction = do
  _      <- char '.'
  (n, l) <- foldM step (0,0) [1..9]
  return $! (+ realToFrac n * 10 ** (- realToFrac l))
  where step :: (Int, Int) -> Int -> Parser (Int, Int)
        step acc@(n,_) l = option acc . try $ do
           c <- digit
           let n' = n * 10 + fromEnum c - 48
           return $! (n', l)

-- |
-- Parse a period.
period :: TimeLocale -> (Text -> Text) -> Parser (Hour -> Hour)
period TimeLocale{amPm = (am, pm)} casify = fromList
  [ (casify $ T.pack am, \ case 12 -> 00; x -> x     )
  , (casify $ T.pack pm, \ case 12 -> 12; x -> x + 12)]

-- |
-- Parse a time zone.
timezone :: City -> Parser TimeZone
timezone city =
  takeWhile1 isAlpha >>=
  either fail return . unabbreviate . TimeZoneAbbr city

-- |
-- Parse a numeric timezone.
offset :: Parser TimeZone
offset = parseOffset

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
