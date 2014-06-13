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

     ) where

import Control.Applicative ((<|>), (*>))
import Control.Arrow ((***))
import Control.Monad (replicateM)
import Data.Attoparsec.Text
import Data.Int (Int64)
import Data.Text (Text, pack, unpack)
import Data.Time.Cube.City (City(..))
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
   | OffsetTime {getOffset :: {-# UNPACK #-} !Int64}
   | PacificDaylightTime
   | PacificStandardTime
   | PakistanStandardTime
   | PakistanSummerTime
   | SingaporeTime
   | SouthAfricaStandardTime
   | WestAfricaTime
   | YukonStandardTime
   deriving (Eq, Generic, Ord, Read, Show)

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
   OffsetTime offset          -> TimeZoneAbbr Universal $ showOffset offset
   PacificDaylightTime        -> TimeZoneAbbr LosAngeles   "PDT"
   PacificStandardTime        -> TimeZoneAbbr LosAngeles   "PST"
   PakistanStandardTime       -> TimeZoneAbbr Karachi      "PKT"
   PakistanSummerTime         -> TimeZoneAbbr Karachi      "PKST"
   SingaporeTime              -> TimeZoneAbbr Singapore    "SGT"
   SouthAfricaStandardTime    -> TimeZoneAbbr Johannesburg "SAST"
   WestAfricaTime             -> TimeZoneAbbr Luanda       "WAT"
   YukonStandardTime          -> TimeZoneAbbr Anchorage    "YST"

-- |
-- Unabbreviate a time zone. An error string is returned for unmatched abbreviations.
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
        _      -> flip parseOnly tzAbbr $ parseOffset <|>
                  fail "unabbreviate: unknown time zone abbreviation string"
        where e = Left "unabbreviate: bad reference location"

-- |
-- Parse a UTC offset string.
parseOffset :: Parser TimeZone
parseOffset = do
   sign    <- plus <|> minus
   hours   <- replicateM 2 digit
   minutes <- replicateM 2 digit
   return $! OffsetTime . sign $ read hours * 60 + read minutes
   where plus  = char '+' *> return id
         minus = char '-' *> return negate

-- |
-- Show a UTC offset.
showOffset :: Int64 -> Text
showOffset offset =
    pack . (:) sign . uncurry (++) . strs $ divMod offset 60
    where sign = if offset < 0 then '-' else '+'
          strs = printf "%02d" . abs *** printf "%02d"
