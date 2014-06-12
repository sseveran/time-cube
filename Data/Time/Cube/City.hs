{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -Wall          #-}

-- |
-- Module      : Data.Time.Cube.City
-- License     : BSD3
-- Maintainer  : Enzo Haussecker
-- Stability   : Stable
-- Portability : Portable
--
-- Location data.
module Data.Time.Cube.City (

 -- ** Cities
       City(..)
     , cities

     ) where

import GHC.Generics (Generic)

-- |
-- Cities from around the world.
data City =
     Aden         -- ^ Yemeni Republic
   | Amman        -- ^ Hashemite Kingdom of Jordan
   | Anchorage    -- ^ United States of America
   | Auckland     -- ^ New Zealand
   | Baghdad      -- ^ Republic of Iraq
   | Berlin       -- ^ Federal Republic of Germany
   | Brussels     -- ^ Kingdom of Belgium
   | Bujumbura    -- ^ Republic of Burundi
   | Cairo        -- ^ Arab Republic of Egypt
   | Chicago      -- ^ United States of America
   | Damascus     -- ^ Syrian Arab Republic
   | Denver       -- ^ United States of America
   | Doha         -- ^ State of Qatar
   | Gaborone     -- ^ Republic of Botswana
   | HongKong     -- ^ People's Republic of China
   | Honolulu     -- ^ United States of America
   | Johannesburg -- ^ Republic of South Africa
   | Kabul        -- ^ Islamic Republic of Afghanistan
   | Karachi      -- ^ Islamic Republic of Pakistan
   | Kinshasa     -- ^ Democratic Republic of the Congo
   | Kolkata      -- ^ Republic of India
   | KuwaitCity   -- ^ State of Kuwait
   | London       -- ^ United Kingdom of Great Britain and Northern Ireland
   | LosAngeles   -- ^ United States of America
   | Luanda       -- ^ Republic of Angola
   | Manama       -- ^ Kingdom of Bahrain
   | Minsk        -- ^ Republic of Belarus
   | Mogadishu    -- ^ Federal Republic of Somalia
   | Moscow       -- ^ Russian Federation
   | NewYork      -- ^ United States of America
   | Oslo         -- ^ Kingdom of Norway
   | Ouagadougou  -- ^ Burkina Faso
   | Paris        -- ^ French Republic
   | Pyongyang    -- ^ Democratic People's Republic of Korea
   | Riyadh       -- ^ Kingdom of Saudi Arabia
   | SaoPaulo     -- ^ Federative Republic of Brazil
   | Sarajevo     -- ^ Bosnia and Herzegovina
   | Seoul        -- ^ Republic of Korea
   | Shanghai     -- ^ People's Republic of China
   | Singapore    -- ^ Republic of Singapore
   | Sofia        -- ^ Republic of Bulgaria
   | Stockholm    -- ^ Kingdom of Sweden
   | Tehran       -- ^ Islamic Republic of Iran
   | TelAviv      -- ^ State of Israel
   | Tirana       -- ^ Republic of Albania
   | Tokyo        -- ^ Japan
   | Toronto      -- ^ Canada
   | Universal    -- ^ International Territory
   | Vienna       -- ^ Republic of Austria
   | Zurich       -- ^ Swiss Confederation
   deriving (Eq, Enum, Generic, Ord, Read, Show)

instance Bounded City where
   minBound = Aden
   maxBound = Zurich

-- |
-- A list of cities in alphabetical order.
cities :: [City]
cities =
   [ Aden
   , Amman
   , Anchorage
   , Auckland
   , Baghdad
   , Berlin
   , Brussels
   , Bujumbura
   , Cairo
   , Chicago
   , Damascus
   , Denver
   , Doha
   , Gaborone
   , HongKong
   , Honolulu
   , Johannesburg
   , Kabul
   , Karachi
   , Kinshasa
   , Kolkata
   , KuwaitCity
   , London
   , LosAngeles
   , Luanda
   , Manama
   , Minsk
   , Mogadishu
   , Moscow
   , NewYork
   , Oslo
   , Ouagadougou
   , Paris
   , Pyongyang
   , Riyadh
   , SaoPaulo
   , Sarajevo
   , Seoul
   , Shanghai
   , Singapore
   , Sofia
   , Stockholm
   , Tehran
   , TelAviv
   , Tirana
   , Tokyo
   , Toronto
   , Universal
   , Vienna
   , Zurich
   ]
