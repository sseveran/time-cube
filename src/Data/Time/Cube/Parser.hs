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
-- Maintainer  : ehaussecker@alphaheavy.com
-- Stability   : Experimental
-- Portability : GHC 7.8.* on Unix
--
-- Timestamp parsing.
module Data.Time.Cube.Parser (

 -- ** Types
       ParserState(..)

 -- ** Parsing
     , parse

     ) where

import Control.Applicative              ((<|>), (<$>), (*>))
import Control.Arrow                    ((***))
import Control.Lens.Setter              (Setter, (%=), assign)
import Control.Lens.TH                  (makeLenses)
import Control.Monad                    ((<=<), foldM, replicateM)
import Control.Monad.State.Strict       (execState, State)
import Data.Attoparsec.Text as P hiding (parse)
import Data.Char                        (isAlpha)
import Data.Text as T                   (Text, length, pack, toLower)
import Data.Time.Cube.Base
import Data.Time.Cube.Format
import Data.Time.Cube.Zones
import Data.Time.Zones                  (TZ)
import Data.Time.Zones.DB               (TZLabel)
import System.Locale                    (TimeLocale(..))

-- |
-- Parser state.
data ParserState (cal :: Calendar) geo = ParserState
  { _ps_year :: Year
  , _ps_mon  :: Month cal
  , _ps_mday :: Day
  , _ps_wday :: DayOfWeek cal
  , _ps_hour :: Hour
  , _ps_min  :: Minute
  , _ps_sec  :: Double
  , _ps_frac :: Double -> Double
  , _ps_ampm :: Hour   -> Hour
  , _ps_zone :: TimeZone geo
  }

makeLenses ''ParserState

-- |
-- Run the generic timestamp parser and return the raw
-- parser state or an error string if the parser failed.
parse
  :: Abbreviate (TimeZone  geo)
  => Bounded    (Month     cal)
  => Enum       (DayOfWeek cal)
  => Enum       (Month     cal)
  => TimeLocale          -- ^ Local Conventions
  -> Maybe (TZLabel, TZ) -- ^ Time Zone Data
  -> ParserState cal geo -- ^ Initialized State
  -> FormatText          -- ^ Format String
  -> Text                -- ^ Input String
  -> Either String (ParserState cal geo)
parse locale mval state format input =
  flip parseOnly input <=< fmap exe . flip parseOnly format . many' $ create locale mval
  where exe sets = flip execState state <$> sequence <$> sequence sets

-- |
-- Create a timestamp parser from local conventions.
create
  :: Abbreviate (TimeZone  geo)
  => Bounded    (Month     cal)
  => Enum       (DayOfWeek cal)
  => Enum       (Month     cal)
  => TimeLocale
  -> Maybe (TZLabel, TZ)
  -> Parser (Parser (State (ParserState cal geo) ()))
create locale mval =

  --- Literals
      percent

  --- Components
  <|> match "%A" ps_wday (weekFull locale)
  <|> match "%B" ps_mon (monthFull locale)
  <|> match "%H" ps_hour (fixed 2)
  <|> match "%I" ps_hour (fixed 2)
  <|> match "%M" ps_min (fixed 2)
  <|> match "%P" ps_ampm (period locale toLower)
  <|> match "%Q" ps_frac fraction
  <|> match "%S" ps_sec second
  <|> match "%Y" ps_year (fixed 4)
  <|> match "%Z" ps_zone (zoneAbbr mval)
  <|> match "%a" ps_wday (weekAbbr locale)
  <|> match "%b" ps_mon (monthAbbr locale)
  <|> match "%d" ps_mday (fixed 2)
  <|> match "%e" ps_mday padded
  <|> match "%h" ps_mon (monthAbbr locale)
  <|> match "%l" ps_hour padded
  <|> match "%m" ps_mon month
  <|> match "%p" ps_ampm (period locale id)
  <|> match "%y" ps_year year
  <|> match "%z" ps_zone zoneOffset

  --- Combinators
  <|> date     "%D" ps_year ps_mon ps_mday
  <|> iso8601  "%F" ps_year ps_mon ps_mday
  <|> clock12  "%r" ps_hour ps_min ps_sec locale
  <|> clock24  "%T" ps_hour ps_min ps_sec
  <|> clock24' "%R" ps_hour ps_min

  --- Text
  <|> text

-- |
-- Match a percent literal.
percent :: Parser (Parser (State (ParserState cal geo) ()))
percent = string "%%" *> return (char '%' *> return (return ()))

-- |
-- Match a percent code and assign
-- the value returned by the parser.
match
  :: Enum (DayOfWeek cal)
  => Enum (Month     cal)
  => Text
  -> Setter (ParserState cal geo) (ParserState cal geo) a a
  -> Parser a
  -> Parser (Parser (State (ParserState cal geo) ()))
match code field parser =
  string code *> return (assign field <$> parser)

-- |
-- Match an ISO 8601 date percent code and
-- assign the values returned by the parser.
iso8601
  :: Bounded (Month cal)
  => Enum    (Month cal)
  => Text
  -> Setter (ParserState cal geo) (ParserState cal geo) Year Year
  -> Setter (ParserState cal geo) (ParserState cal geo) (Month cal) (Month cal)
  -> Setter (ParserState cal geo) (ParserState cal geo) Day Day
  -> Parser (Parser (State (ParserState cal geo) ()))
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
  => Enum    (Month cal)
  => Text
  -> Setter (ParserState cal geo) (ParserState cal geo) Year Year
  -> Setter (ParserState cal geo) (ParserState cal geo) (Month cal) (Month cal)
  -> Setter (ParserState cal geo) (ParserState cal geo) Day Day
  -> Parser (Parser (State (ParserState cal geo) ()))
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
  -> Setter (ParserState cal geo) (ParserState cal geo) Hour Hour
  -> Setter (ParserState cal geo) (ParserState cal geo) Minute Minute
  -> Setter (ParserState cal geo) (ParserState cal geo) Double Double
  -> TimeLocale
  -> Parser (Parser (State (ParserState cal geo) ()))
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
  -> Setter (ParserState cal geo) (ParserState cal geo) Hour Hour
  -> Setter (ParserState cal geo) (ParserState cal geo) Minute Minute
  -> Setter (ParserState cal geo) (ParserState cal geo) Double Double
  -> Parser (Parser (State (ParserState cal geo) ()))
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
  -> Setter (ParserState cal geo) (ParserState cal geo) Hour Hour
  -> Setter (ParserState cal geo) (ParserState cal geo) Minute Minute
  -> Parser (Parser (State (ParserState cal geo) ()))
clock24' code hour min =
  string code *> return parser
  where parser = do
          h <- fixed 2; _ <- char ':'
          m <- fixed 2
          return $! assign hour h *>
                    assign min  m

-- |
-- Match any other character sequence.
text :: Parser (Parser (State (ParserState cal geo) ()))
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
  , (casify $ T.pack pm, \ case 12 -> 12; x -> x + 12) ]

-- |
-- Parse a time zone in short text format.
zoneAbbr :: Abbreviate (TimeZone geo) => Maybe (TZLabel, TZ) -> Parser (TimeZone geo)
zoneAbbr mval = takeWhile1 isAlpha >>= either fail return . unabbreviate mval

-- |
-- Parse a time zone in offset text format.
zoneOffset :: Abbreviate (TimeZone geo) => Parser (TimeZone geo)
zoneOffset = P.take 5 >>= either fail return . unabbreviate Nothing

-- |
-- Parse a month in short text format.
monthAbbr :: Enum (Month cal) => TimeLocale -> Parser (Month cal)
monthAbbr = fromList . zipWith (\n (_, abbr) -> (T.pack abbr, toEnum n)) [1..] . months

-- |
-- Parse a month in long text format.
monthFull :: Enum (Month cal) => TimeLocale -> Parser (Month cal)
monthFull = fromList . zipWith (\n (full, _) -> (T.pack full, toEnum n)) [1..] . months

-- |
-- Parse a day of week in short text format.
weekAbbr :: Enum (DayOfWeek cal) => TimeLocale -> Parser (DayOfWeek cal)
weekAbbr = fromList . zipWith (\n (_, abbr) -> (T.pack abbr, toEnum n)) [1..] . wDays

-- |
-- Parse a day of week in long text format. 
weekFull :: Enum (DayOfWeek cal) => TimeLocale -> Parser (DayOfWeek cal)
weekFull = fromList . zipWith (\n (full, _) -> (T.pack full, toEnum n)) [1..] . wDays

-- |
-- Create a parser from a list of key-value pairs.
fromList :: [(Text, a)] -> Parser a
fromList = foldl1 (<|>) . map (uncurry (*>) . (string *** return))