{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS -Wall              #-}

-- |
-- Module      : Data.Time.Cube.Zone
-- License     : BSD3
-- Maintainer  : Enzo Haussecker
-- Stability   : Stable
-- Portability : Portable
--
-- Time zone data.
module Data.Time.Cube.Zone (

 -- ** Time Zones
       TimeZone(..)
     , utc

 -- ** Abbreviations
     , TimeZoneAbbr(..)
     , abbreviate
     , unabbreviate

 -- ** Utilities
     , getUTCOffset
     , parseUTCOffset

     ) where

import Control.Applicative ((<|>), (<$>), (*>))
import Control.Arrow ((***))
import Control.DeepSeq (NFData(..))
import Control.Monad (replicateM)
import Data.Attoparsec.Text (Parser, char, digit, option, parseOnly, try)
import Data.Int (Int16)
import Data.Text (Text, pack, unpack)
import Data.Time.Cube.City (City(..))
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Text.Printf (printf)

-- |
-- Time zones.
data TimeZone =
     AfghanistanTime
   | AlaskaDaylightTime
   | AlaskaHawaiiDaylightTime
   | AlaskaHawaiiStandardTime
   | AlaskaStandardTime
   | ArabiaDaylightTime
   | ArabiaStandardTime
   | BrasiliaSummerTime
   | BrasiliaTime
   | BritishSummerTime
   | CentralAfricaTime
   | CentralDaylightTime
   | CentralEuropeanSummerTime
   | CentralEuropeanTime
   | CentralStandardTime
   | ChinaDaylightTime
   | ChinaStandardTime
   | CoordinatedUniversalTime
   | EastAfricaTime
   | EasternDaylightTime
   | EasternEuropeanSummerTime
   | EasternEuropeanTime
   | EasternStandardTime
   | FurtherEasternEuropeanTime
   | GreenwichMeanTime
   | GulfStandardTime
   | HawaiiAleutianStandardTime
   | HongKongSummerTime
   | HongKongTime
   | IndiaStandardTime
   | IranDaylightTime
   | IranStandardTime
   | IsraelDaylightTime
   | IsraelStandardTime
   | JapanStandardTime
   | KarachiTime
   | KoreaDaylightTime
   | KoreaStandardTime
   | MoscowDaylightTime
   | MoscowStandardTime
   | MountainDaylightTime
   | MountainStandardTime
   | NewZealandDaylightTime
   | NewZealandStandardTime
   | OffsetTime {-# UNPACK #-} !Int16
   | PacificDaylightTime
   | PacificStandardTime
   | PakistanStandardTime
   | PakistanSummerTime
   | SingaporeTime
   | SouthAfricaStandardTime
   | WestAfricaTime
   | YukonStandardTime
   deriving (Eq, Generic, Ord, Read, Show)

instance NFData TimeZone where
   rnf (OffsetTime offset) = rnf offset `seq` ()
   rnf _                   = ()

instance Storable TimeZone where
   sizeOf  _ = 2
   alignment = sizeOf
   peekElemOff ptr n = OffsetTime <$> peekElemOff (castPtr ptr) n
   pokeElemOff ptr n = pokeElemOff (castPtr ptr) n . getUTCOffset

-- |
-- The UTC time zone.
utc :: TimeZone
utc = CoordinatedUniversalTime

-- |
-- Time zone abbreviation.
data TimeZoneAbbr = TimeZoneAbbr {
     tzCity :: City -- ^ Reference location
   , tzAbbr :: Text -- ^ Time zone abbreviation string
   } deriving (Eq, Generic)

instance Show TimeZoneAbbr where
   show TimeZoneAbbr{tzAbbr} = unpack tzAbbr

-- |
-- Abbreviate a time zone.
abbreviate :: TimeZone -> TimeZoneAbbr
abbreviate = \ case
   AfghanistanTime            -> TimeZoneAbbr Kabul        "AFT"
   AlaskaDaylightTime         -> TimeZoneAbbr Anchorage    "AKDT"
   AlaskaHawaiiDaylightTime   -> TimeZoneAbbr Anchorage    "AHDT"
   AlaskaHawaiiStandardTime   -> TimeZoneAbbr Anchorage    "AHST"
   AlaskaStandardTime         -> TimeZoneAbbr Anchorage    "AKST"
   ArabiaDaylightTime         -> TimeZoneAbbr Baghdad      "ADT"
   ArabiaStandardTime         -> TimeZoneAbbr Riyadh       "AST"
   BrasiliaSummerTime         -> TimeZoneAbbr SaoPaulo     "BRST"
   BrasiliaTime               -> TimeZoneAbbr SaoPaulo     "BRT"
   BritishSummerTime          -> TimeZoneAbbr London       "BST"
   CentralAfricaTime          -> TimeZoneAbbr Gaborone     "CAT"
   CentralDaylightTime        -> TimeZoneAbbr Chicago      "CDT"
   CentralEuropeanSummerTime  -> TimeZoneAbbr Paris        "CEST"
   CentralEuropeanTime        -> TimeZoneAbbr Paris        "CET"
   CentralStandardTime        -> TimeZoneAbbr Chicago      "CST"
   ChinaDaylightTime          -> TimeZoneAbbr Shanghai     "CDT"
   ChinaStandardTime          -> TimeZoneAbbr Shanghai     "CST"
   CoordinatedUniversalTime   -> TimeZoneAbbr Universal    "UTC"
   EastAfricaTime             -> TimeZoneAbbr Mogadishu    "EAT"
   EasternDaylightTime        -> TimeZoneAbbr NewYork      "EDT"
   EasternEuropeanSummerTime  -> TimeZoneAbbr Sofia        "EEST"
   EasternEuropeanTime        -> TimeZoneAbbr Sofia        "EET"
   EasternStandardTime        -> TimeZoneAbbr NewYork      "EST"
   FurtherEasternEuropeanTime -> TimeZoneAbbr Minsk        "FET"
   GreenwichMeanTime          -> TimeZoneAbbr London       "GMT"
   GulfStandardTime           -> TimeZoneAbbr Manama       "GST"
   HawaiiAleutianStandardTime -> TimeZoneAbbr Honolulu     "HST"
   HongKongSummerTime         -> TimeZoneAbbr HongKong     "HKST"
   HongKongTime               -> TimeZoneAbbr HongKong     "HKT"
   IndiaStandardTime          -> TimeZoneAbbr Kolkata      "IST"
   IranDaylightTime           -> TimeZoneAbbr Tehran       "IRDT"
   IranStandardTime           -> TimeZoneAbbr Tehran       "IRST"
   IsraelDaylightTime         -> TimeZoneAbbr TelAviv      "IDT"
   IsraelStandardTime         -> TimeZoneAbbr TelAviv      "IST"
   JapanStandardTime          -> TimeZoneAbbr Tokyo        "JST"
   KarachiTime                -> TimeZoneAbbr Karachi      "KART"
   KoreaDaylightTime          -> TimeZoneAbbr Seoul        "KDT"
   KoreaStandardTime          -> TimeZoneAbbr Seoul        "KST"
   MoscowDaylightTime         -> TimeZoneAbbr Moscow       "MSD"
   MoscowStandardTime         -> TimeZoneAbbr Moscow       "MSK"
   MountainDaylightTime       -> TimeZoneAbbr Denver       "MDT"
   MountainStandardTime       -> TimeZoneAbbr Denver       "MST"
   NewZealandDaylightTime     -> TimeZoneAbbr Auckland     "NZDT"
   NewZealandStandardTime     -> TimeZoneAbbr Auckland     "NZST"
   OffsetTime offset          -> TimeZoneAbbr Universal $ showUTCOffset offset
   PacificDaylightTime        -> TimeZoneAbbr LosAngeles   "PDT"
   PacificStandardTime        -> TimeZoneAbbr LosAngeles   "PST"
   PakistanStandardTime       -> TimeZoneAbbr Karachi      "PKT"
   PakistanSummerTime         -> TimeZoneAbbr Karachi      "PKST"
   SingaporeTime              -> TimeZoneAbbr Singapore    "SGT"
   SouthAfricaStandardTime    -> TimeZoneAbbr Johannesburg "SAST"
   WestAfricaTime             -> TimeZoneAbbr Luanda       "WAT"
   YukonStandardTime          -> TimeZoneAbbr Anchorage    "YST"

-- |
-- Unabbreviate a time zone abbreviation. An error
-- string is returned for unmatched abbreviations.
unabbreviate :: TimeZoneAbbr -> Either String TimeZone
unabbreviate TimeZoneAbbr{..} = 
   case tzAbbr of
        "AFT"  -> Right AfghanistanTime
        "AHDT" -> Right AlaskaHawaiiDaylightTime
        "AHST" -> Right AlaskaHawaiiStandardTime
        "AKDT" -> Right AlaskaDaylightTime
        "AKST" -> Right AlaskaStandardTime
        "ADT"  -> Right ArabiaDaylightTime
        "AST"  -> Right ArabiaStandardTime
        "BRST" -> Right BrasiliaSummerTime
        "BRT"  -> Right BrasiliaTime
        "BST"  -> Right BritishSummerTime
        "CAT"  -> Right CentralAfricaTime
        "CDT"  -> case tzCity of
                       Chicago  -> Right CentralDaylightTime
                       Shanghai -> Right ChinaDaylightTime
                       _        -> e
        "CEST" -> Right CentralEuropeanSummerTime
        "CET"  -> Right CentralEuropeanTime
        "CST"  -> case tzCity of
                       Chicago  -> Right CentralStandardTime
                       Shanghai -> Right ChinaStandardTime
                       _        -> e
        "EAT"  -> Right EastAfricaTime
        "EDT"  -> Right EasternDaylightTime
        "EEST" -> Right EasternEuropeanSummerTime
        "EET"  -> Right EasternEuropeanTime
        "EST"  -> Right EasternStandardTime
        "FET"  -> Right FurtherEasternEuropeanTime
        "GMT"  -> Right GreenwichMeanTime
        "GST"  -> Right GulfStandardTime
        "HST"  -> Right HawaiiAleutianStandardTime
        "HKST" -> Right HongKongSummerTime
        "HKT"  -> Right HongKongTime
        "IDT"  -> Right IsraelDaylightTime
        "IRDT" -> Right IranDaylightTime
        "IRST" -> Right IranStandardTime
        "IST"  -> case tzCity of
                       Kolkata -> Right IndiaStandardTime
                       TelAviv -> Right IsraelStandardTime
                       _       -> e
        "JST"  -> Right JapanStandardTime
        "KART" -> Right KarachiTime
        "KDT"  -> Right KoreaDaylightTime
        "KST"  -> Right KoreaStandardTime
        "MDT"  -> Right MountainDaylightTime
        "MSD"  -> Right MoscowDaylightTime
        "MSK"  -> Right MoscowStandardTime
        "MST"  -> Right MountainStandardTime
        "NZDT" -> Right NewZealandDaylightTime
        "NZST" -> Right NewZealandStandardTime
        "PDT"  -> Right PacificDaylightTime
        "PKST" -> Right PakistanSummerTime
        "PKT"  -> Right PakistanStandardTime
        "PST"  -> Right PacificStandardTime
        "SAST" -> Right SouthAfricaStandardTime
        "SGT"  -> Right SingaporeTime
        "UTC"  -> Right CoordinatedUniversalTime
        "WAT"  -> Right WestAfricaTime
        "YST"  -> Right YukonStandardTime
        _      -> flip parseOnly tzAbbr $ parseUTCOffset <|>
                  fail "unabbreviate: unknown time zone abbreviation string"
        where e = Left "unabbreviate: bad reference location"

-- |
-- Get the UTC offset (in minutes) for the given time zone.
getUTCOffset :: TimeZone -> Int16
getUTCOffset = \ case
   AfghanistanTime            ->  270
   AlaskaDaylightTime         -> -480
   AlaskaHawaiiDaylightTime   -> -540
   AlaskaHawaiiStandardTime   -> -600
   AlaskaStandardTime         -> -540
   ArabiaDaylightTime         ->  240
   ArabiaStandardTime         ->  180
   BrasiliaSummerTime         -> -120
   BrasiliaTime               -> -180
   BritishSummerTime          ->  060
   CentralAfricaTime          ->  120
   CentralDaylightTime        -> -300
   CentralEuropeanSummerTime  ->  120
   CentralEuropeanTime        ->  060
   CentralStandardTime        -> -360
   ChinaDaylightTime          ->  540
   ChinaStandardTime          ->  480
   CoordinatedUniversalTime   ->  000
   EastAfricaTime             ->  180
   EasternDaylightTime        -> -240
   EasternEuropeanSummerTime  ->  180
   EasternEuropeanTime        ->  120
   EasternStandardTime        -> -300
   FurtherEasternEuropeanTime ->  180
   GreenwichMeanTime          ->  000
   GulfStandardTime           ->  240
   HawaiiAleutianStandardTime -> -600
   HongKongSummerTime         ->  540
   HongKongTime               ->  480
   IndiaStandardTime          ->  330
   IranDaylightTime           ->  270
   IranStandardTime           ->  210
   IsraelDaylightTime         ->  180
   IsraelStandardTime         ->  120
   JapanStandardTime          ->  540
   KarachiTime                ->  300
   KoreaDaylightTime          ->  600
   KoreaStandardTime          ->  540
   MoscowDaylightTime         ->  240
   MoscowStandardTime         ->  240
   MountainDaylightTime       -> -360
   MountainStandardTime       -> -420
   NewZealandDaylightTime     ->  780
   NewZealandStandardTime     ->  720
   OffsetTime offset          -> offset
   PacificDaylightTime        -> -420
   PacificStandardTime        -> -480
   PakistanStandardTime       ->  300
   PakistanSummerTime         ->  360
   SingaporeTime              ->  480
   SouthAfricaStandardTime    ->  120
   WestAfricaTime             ->  060
   YukonStandardTime          -> -540

-- |
-- Parse a UTC offset string.
parseUTCOffset :: Parser TimeZone
parseUTCOffset = do
   sign    <- plus <|> minus
   hours   <- replicateM 2 digit
   _       <- try . option ':' $ char ':'
   minutes <- replicateM 2 digit
   return $! OffsetTime . sign $ read hours * 60 + read minutes
   where plus  = char '+' *> return id
         minus = char '-' *> return negate

-- |
-- Show a UTC offset.
showUTCOffset :: Int16 -> Text
showUTCOffset offset =
    pack . (:) sign . pretty . flip divMod 60 $ abs offset
    where sign   = if offset < 0 then '-' else '+'
          pretty = uncurry (++) . (printf "%02d" *** (:) ':' . printf "%02d")
