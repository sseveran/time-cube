{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -Wall          #-}

-- |
-- Module      : Data.Time.Cube.Zone
-- Copyright   : Copyright (c) 2014, Alpha Heavy Industries, Inc. All rights reserved.
-- License     : Apache License, Version 2.0
-- Maintainer  : Enzo Haussecker <enzo@ucsd.edu>
-- Stability   : Stable
-- Portability : Portable
--
-- Time zone data types.
module Data.Time.Cube.Zone (

 -- ** Time Zones
       TimeZone(..)

     ) where

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
   | Unspecified
   | WestAfricaTime
   | YukonStandardTime
   deriving (Eq, Enum, Generic, Ord, Read, Show)
