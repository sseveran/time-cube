{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall                  #-}

-- |
-- Module      : Data.Time.Cube.Zones
-- License     : BSD3
-- Maintainer  : Enzo Haussecker
-- Stability   : Stable
-- Portability : Portable
--
-- Dependently typed time zones and related utilities.
module Data.Time.Cube.Zones (

 -- ** Time Zones
       TimeZone(..)
     , utc

 -- ** Geography
     , Universal
     , Offset
     , Olson(..)

 -- ** Abbreviations
     , Abbreviate(..)

 -- ** Conversions
     , Convert(..)
     , ConvertError(..)

 -- ** GHC Extensions
     , SigNat(..)
     , KnownSigNat(..)

     ) where

import Control.Applicative ((<|>), (<$>), (*>))
import Control.DeepSeq (NFData(..))
import Control.Monad (replicateM)
import Data.Attoparsec.Text (Parser, char, digit, option, parseOnly, try)
import Data.Int (Int16)
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat, Symbol, natVal)
import Text.Printf (printf)

-- |
-- A uniform standard for time with geographic parametrization.
data family TimeZone :: * -> *

-- |
-- The coordinated universal time zone.
utc :: TimeZone Universal
utc = CoordinatedUniversalTime

-- |
-- A geographic region that observes coordinated universal time.
data Universal

-- |
-- A geographic region characterized by its offset in minutes from coordinated universal time.
data Offset :: SigNat -> *

-- |
-- A geographic region defined in the Olson database.
data family Olson :: Symbol -> *

data instance TimeZone Universal =
     CoordinatedUniversalTime
     deriving (Eq, Generic, Show)

data instance TimeZone (Offset int) =
     Offset { getOffset :: {-# UNPACK #-} !Int16 }
     deriving (Eq, Generic, Show)

data instance TimeZone (Olson "America/Anchorage") =
     AlaskaStandardTime
   | AlaskaDaylightTime
     deriving (Eq, Generic, Show)

data instance TimeZone (Olson "America/Chicago") =
     CentralStandardTime
   | CentralDaylightTime
     deriving (Eq, Generic, Show)

data instance TimeZone (Olson "America/Denver") =
     MountainStandardTime
   | MountainDaylightTime
     deriving (Eq, Generic, Show)

data instance TimeZone (Olson "America/Los_Angeles") =
     PacificStandardTime
   | PacificDaylightTime
     deriving (Eq, Generic, Show)

data instance TimeZone (Olson "America/New_York") =
     EasternStandardTime
   | EasternDaylightTime
     deriving (Eq, Generic, Show)

data instance TimeZone (Olson "Asia/Kabul") =
     AfghanistanTime
     deriving (Eq, Generic, Show)

data instance (Olson "America/Anchorage") = Anchorage
     deriving (Eq, Generic)

data instance (Olson "America/Chicago") = Chicago
     deriving (Eq, Generic)

data instance (Olson "America/Denver") = Denver
     deriving (Eq, Generic)

data instance (Olson "America/Los_Angeles") = LosAngeles
     deriving (Eq, Generic)

data instance (Olson "America/New_York") = NewYork
     deriving (Eq, Generic)

data instance (Olson "Asia/Kabul") = Kabul
     deriving (Eq, Generic)

instance Show (Olson "America/Anchorage") where

    show Anchorage = "America/Anchorage"

instance Show (Olson "America/Chicago") where

    show Chicago = "America/Chicago"

instance Show (Olson "America/Denver") where

    show Denver = "America/Denver"

instance Show (Olson "America/Los_Angeles") where

    show LosAngeles = "America/Los_Angeles"

instance Show (Olson "America/New_York") where

    show NewYork = "America/New_York"

instance Show (Olson "Asia/Kabul") where

    show Kabul = "Asia/Kabul"

class Abbreviate a where

   -- |
   -- Get the abbreviation text for the given time zone.
   abbreviate :: TimeZone a -> Text

   -- |
   -- Get the time zone for the given abbreviation text.
   unabbreviate :: Text -> Either String (TimeZone a)

instance Abbreviate Universal where

   abbreviate CoordinatedUniversalTime = "UTC"

   unabbreviate = \ case
     "UTC" -> Right CoordinatedUniversalTime
     txt   -> unknown "Universal" txt

instance KnownSigNat int => Abbreviate (Offset int) where

   abbreviate = pack . showOffset

   unabbreviate = parseOnly parseOffset

instance Abbreviate (Olson "America/Anchorage") where

   abbreviate = \ case
     AlaskaStandardTime -> "AKST"
     AlaskaDaylightTime -> "AKDT"

   unabbreviate = \ case
     "AKST" -> Right AlaskaStandardTime
     "AKDT" -> Right AlaskaDaylightTime
     txt    -> unknown "(Olson \"America/Anchorage\")" txt

instance Abbreviate (Olson "America/Chicago") where

   abbreviate = \ case
     CentralStandardTime -> "CST"
     CentralDaylightTime -> "CDT"

   unabbreviate = \ case
     "CST" -> Right CentralStandardTime
     "CDT" -> Right CentralDaylightTime
     txt   -> unknown "(Olson \"America/Chicago\")" txt

instance Abbreviate (Olson "America/Denver") where

   abbreviate = \ case
     MountainStandardTime -> "MST"
     MountainDaylightTime -> "MDT"

   unabbreviate = \ case
     "MST" -> Right MountainStandardTime
     "MDT" -> Right MountainDaylightTime
     txt   -> unknown "(Olson \"America/Denver\")" txt

instance Abbreviate (Olson "America/Los_Angeles") where

   abbreviate = \ case
     PacificStandardTime -> "PST"
     PacificDaylightTime -> "PDT"

   unabbreviate = \ case
     "PST" -> Right PacificStandardTime
     "PDT" -> Right PacificDaylightTime
     txt   -> unknown "(Olson \"America/Los_Angeles\")" txt

instance Abbreviate (Olson "America/New_York") where

   abbreviate = \ case
     EasternStandardTime -> "EST"
     EasternDaylightTime -> "EDT"

   unabbreviate = \ case
     "EST" -> Right EasternStandardTime
     "EDT" -> Right EasternDaylightTime
     txt   -> unknown "(Olson \"America/New_York\")" txt

instance Abbreviate (Olson "Asia/Kabul") where

   abbreviate AfghanistanTime = "AFT"

   unabbreviate = \ case
     "AFT" -> Right AfghanistanTime
     txt   -> unknown "(Olson \"Asia/Kabul\")" txt

-- |
-- Fail for an unmatched time zone abbreviation string.
unknown :: String -> Text -> Either String a
unknown geo txt = Left $ "unabbreviate{TimeZone " ++ geo ++ "}: unmatched time zone abbreviation string " ++ show txt

-- |
-- Convert from one time zone to another.
class Convert a b where

   convert :: TimeZone a -> Either ConvertError (TimeZone b)

-- |
-- How we indicate that an error occurred.
data ConvertError =
     ConvertError
     { srcVal  :: String
     , srcType :: String
     , trgType :: String
     , errMsg  :: String
     } deriving Show

instance Convert Universal (Offset (Plus 0)) where

   convert CoordinatedUniversalTime = Right $ Offset 0

instance Convert Universal (Offset (Minus 0)) where

   convert CoordinatedUniversalTime = Right $ Offset 0

instance Convert (Olson "America/Anchorage") (Offset (Minus 540)) where

   convert AlaskaStandardTime = Right $ Offset (-540)
   convert tz                 = Left  $ ConvertError (show tz) "Olson \"America/Anchorage\"" "Offset (Minus 540)" "unmatched time zone"

instance Convert (Olson "America/Anchorage") (Offset (Minus 480)) where

   convert AlaskaDaylightTime = Right $ Offset (-480)
   convert tz                 = Left  $ ConvertError (show tz) "Olson \"America/Anchorage\"" "Offset (Minus 480)" "unmatched time zone"

instance Convert (Olson "America/Chicago") (Offset (Minus 360)) where

   convert CentralStandardTime = Right $ Offset (-360)
   convert tz                  = Left  $ ConvertError (show tz) "Olson \"America/Chicago\"" "Offset (Minus 360)" "unmatched time zone"

instance Convert (Olson "America/Chicago") (Offset (Minus 300)) where

   convert CentralDaylightTime = Right $ Offset (-300)
   convert tz                  = Left  $ ConvertError (show tz) "Olson \"America/Chicago\"" "Offset (Minus 300)" "unmatched time zone"

instance Convert (Olson "America/Denver") (Offset (Minus 420)) where

   convert MountainStandardTime = Right $ Offset (-420)
   convert tz                   = Left  $ ConvertError (show tz) "Olson \"America/Denver\"" "Offset (Minus 420)" "unmatched time zone"

instance Convert (Olson "America/Denver") (Offset (Minus 360)) where

   convert MountainDaylightTime = Right $ Offset (-360)
   convert tz                   = Left  $ ConvertError (show tz) "Olson \"America/Denver\"" "Offset (Minus 360)" "unmatched time zone"

instance Convert (Olson "America/Los_Angeles") (Offset (Minus 480)) where

   convert PacificStandardTime = Right $ Offset (-480)
   convert tz                  = Left  $ ConvertError (show tz) "Olson \"America/Los_Angeles\"" "Offset (Minus 480)" "unmatched time zone"

instance Convert (Olson "America/Los_Angeles") (Offset (Minus 420)) where

   convert PacificDaylightTime = Right $ Offset (-420)
   convert tz                  = Left  $ ConvertError (show tz) "Olson \"America/Los_Angeles\"" "Offset (Minus 420)" "unmatched time zone"

instance Convert (Olson "America/New_York") (Offset (Minus 300)) where

   convert EasternStandardTime = Right $ Offset (-300)
   convert tz                  = Left  $ ConvertError (show tz) "Olson \"America/New_York\"" "Offset (Minus 300)" "unmatched time zone"

instance Convert (Olson "America/New_York") (Offset (Minus 240)) where

   convert EasternDaylightTime = Right $ Offset (-240)
   convert tz                  = Left  $ ConvertError (show tz) "Olson \"America/New_York\"" "Offset (Minus 240)" "unmatched time zone"

instance Convert (Olson "Asia/Kabul") (Offset (Plus 270)) where

   convert AfghanistanTime = Right $ Offset 270

instance NFData (TimeZone Universal) where

   rnf _ = ()

instance NFData (TimeZone (Offset int)) where

   rnf Offset{..} = rnf getOffset `seq` ()

instance NFData (TimeZone (Olson file)) where

   rnf _ = ()

instance Storable (TimeZone (Offset int)) where

   sizeOf  _ = 2
   alignment = sizeOf

   peekElemOff ptr n = Offset <$> peekElemOff (castPtr ptr) n
   pokeElemOff ptr n = pokeElemOff (castPtr ptr) n . getOffset

-- |
-- Parse a time zone offset string.
parseOffset :: forall int . KnownSigNat int => Parser (TimeZone (Offset int))
parseOffset = do
   sign  <- plus <|> minus
   hours <- replicateM 2 digit
   _     <- try . option ':' $ char ':'
   mins  <- replicateM 2 digit
   let value = sign $ read hours * 60 + read mins
       proxy = Proxy :: Proxy int
   if value == sigNatVal proxy
   then return $! Offset $ fromInteger value
   else fail "parseOffset: type dependency not satisfied"
   where plus  = char '+' *> return id
         minus = char '-' *> return negate

-- |
-- Show a time zone offset string.
showOffset :: TimeZone (Offset int) -> String
showOffset Offset{..} =
   sign : hours ++ ':' : mins
   where sign  = if getOffset < 0 then '-' else '+'
         hours = printf "%02d" $ div value 60
         mins  = printf "%02d" $ mod value 60
         value = abs getOffset

-- |
-- A generalized algebraic data type
-- representing signed type-level naturals.
data SigNat where
     Plus  :: Nat -> SigNat
     Minus :: Nat -> SigNat

-- |
-- Get the integer associated with the
-- signed type-level natural via proxy.
class KnownSigNat (int :: SigNat) where

   sigNatVal :: Proxy int -> Integer

instance KnownNat nat => KnownSigNat (Plus nat) where

   sigNatVal _ = natVal (Proxy :: Proxy nat)

instance KnownNat nat => KnownSigNat (Minus nat) where

   sigNatVal _ = negate $ natVal (Proxy :: Proxy nat)
