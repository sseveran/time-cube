{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall            #-}

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
     , unabbreviate
     , abbreviate

     ) where

import Data.Map.Strict ((!), fromDistinctAscList)
import Data.Time.Cube.City (City(..))
import GHC.Generics (Generic)

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
   | PacificDaylightTime
   | PacificStandardTime
   | PakistanStandardTime
   | PakistanSummerTime
   | SingaporeTime
   | SouthAfricaStandardTime
   | WestAfricaTime
   | YukonStandardTime
   deriving (Eq, Enum, Generic, Ord, Read, Show)

-- |
-- The UTC time zone.
utc :: TimeZone
utc = CoordinatedUniversalTime

-- |
-- Time zone abbreviation.
data TimeZoneAbbr = TimeZoneAbbr {
     tzCity :: City   -- ^ Reference location
   , tzAbbr :: String -- ^ Time zone abbreviation string
   } deriving (Eq, Generic)

instance Show TimeZoneAbbr where
   show TimeZoneAbbr{tzAbbr} = tzAbbr

-- |
-- Unabbreviate a time zone.
unabbreviate :: TimeZoneAbbr -> Either String TimeZone
unabbreviate (TimeZoneAbbr city abbr) = 
   case abbr of
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
        "CDT"  -> case city of
                       Chicago  -> Right CentralDaylightTime
                       Shanghai -> Right ChinaDaylightTime
                       _        -> f
        "CEST" -> Right CentralEuropeanSummerTime
        "CET"  -> Right CentralEuropeanTime
        "CST"  -> case city of
                       Chicago  -> Right CentralStandardTime
                       Shanghai -> Right ChinaStandardTime
                       _        -> f
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
        "IST"  -> case city of
                       Kolkata -> Right IndiaStandardTime
                       TelAviv -> Right IsraelStandardTime
                       _       -> f
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
        _      -> Left $ "unabbreviate: unknown time zone abbreviation string"
        where f = Left $ "unabbreviate: bad reference location"

-- |
-- Abbreviate a time zone.
abbreviate :: TimeZone -> TimeZoneAbbr
abbreviate = (!) $ fromDistinctAscList
   [ (AfghanistanTime,TimeZoneAbbr Kabul "AFT")
   , (AlaskaDaylightTime,TimeZoneAbbr Anchorage "AKDT")
   , (AlaskaHawaiiDaylightTime,TimeZoneAbbr Anchorage "AHDT")
   , (AlaskaHawaiiStandardTime,TimeZoneAbbr Anchorage "AHST")
   , (AlaskaStandardTime,TimeZoneAbbr Anchorage "AKST")
   , (ArabiaDaylightTime,TimeZoneAbbr Baghdad "ADT")
   , (ArabiaStandardTime,TimeZoneAbbr Riyadh "AST")
   , (BrasiliaSummerTime,TimeZoneAbbr SaoPaulo "BRST")
   , (BrasiliaTime,TimeZoneAbbr SaoPaulo "BRT")
   , (BritishSummerTime,TimeZoneAbbr London "BST")
   , (CentralAfricaTime,TimeZoneAbbr Gaborone "CAT")
   , (CentralDaylightTime,TimeZoneAbbr Chicago "CDT")
   , (CentralEuropeanSummerTime,TimeZoneAbbr Paris "CEST")
   , (CentralEuropeanTime,TimeZoneAbbr Paris "CET")
   , (CentralStandardTime,TimeZoneAbbr Chicago "CST")
   , (ChinaDaylightTime,TimeZoneAbbr Shanghai "CDT")
   , (ChinaStandardTime,TimeZoneAbbr Shanghai "CST")
   , (CoordinatedUniversalTime,TimeZoneAbbr Universal "UTC")
   , (EastAfricaTime,TimeZoneAbbr Mogadishu "EAT")
   , (EasternDaylightTime,TimeZoneAbbr NewYork "EDT")
   , (EasternEuropeanSummerTime,TimeZoneAbbr Sofia "EEST")
   , (EasternEuropeanTime,TimeZoneAbbr Sofia "EET")
   , (EasternStandardTime,TimeZoneAbbr NewYork "EST")
   , (FurtherEasternEuropeanTime,TimeZoneAbbr Minsk "FET")
   , (GreenwichMeanTime,TimeZoneAbbr London "GMT")
   , (GulfStandardTime,TimeZoneAbbr Manama "GST")
   , (HawaiiAleutianStandardTime,TimeZoneAbbr Honolulu "HST")
   , (HongKongSummerTime,TimeZoneAbbr HongKong "HKST")
   , (HongKongTime,TimeZoneAbbr HongKong "HKT")
   , (IndiaStandardTime,TimeZoneAbbr Kolkata "IST")
   , (IranDaylightTime,TimeZoneAbbr Tehran "IRDT")
   , (IranStandardTime,TimeZoneAbbr Tehran "IRST")
   , (IsraelDaylightTime,TimeZoneAbbr TelAviv "IDT")
   , (IsraelStandardTime,TimeZoneAbbr TelAviv "IST")
   , (JapanStandardTime,TimeZoneAbbr Tokyo "JST")
   , (KarachiTime,TimeZoneAbbr Karachi "KART")
   , (KoreaDaylightTime,TimeZoneAbbr Seoul "KDT")
   , (KoreaStandardTime,TimeZoneAbbr Seoul "KST")
   , (MoscowDaylightTime,TimeZoneAbbr Moscow "MSD")
   , (MoscowStandardTime,TimeZoneAbbr Moscow "MSK")
   , (MountainDaylightTime,TimeZoneAbbr Denver "MDT")
   , (MountainStandardTime,TimeZoneAbbr Denver "MST")
   , (NewZealandDaylightTime,TimeZoneAbbr Auckland "NZDT")
   , (NewZealandStandardTime,TimeZoneAbbr Auckland "NZST")
   , (PacificDaylightTime,TimeZoneAbbr LosAngeles "PDT")
   , (PacificStandardTime,TimeZoneAbbr LosAngeles "PST")
   , (PakistanStandardTime,TimeZoneAbbr Karachi "PKT")
   , (PakistanSummerTime,TimeZoneAbbr Karachi "PKST")
   , (SingaporeTime,TimeZoneAbbr Singapore "SGT")
   , (SouthAfricaStandardTime,TimeZoneAbbr Johannesburg "SAST")
   , (WestAfricaTime,TimeZoneAbbr Luanda "WAT")
   , (YukonStandardTime,TimeZoneAbbr Anchorage "YST")
   ]
