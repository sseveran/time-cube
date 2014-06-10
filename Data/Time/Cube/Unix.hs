{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall                       #-}

-- |
-- Module      : Data.Time.Cube.Unix
-- License     : BSD3
-- Maintainer  : Enzo Haussecker
-- Stability   : Stable
-- Portability : Portable
--
-- Unix timestamps.
module Data.Time.Cube.Unix (

 -- ** Types
       UnixDate(..)
     , UnixTime(..)

     ) where

import Control.DeepSeq (NFData)
import Data.Int (Int32, Int64)
import Data.Time.Cube.Base (Calendar)
import Data.Time.Cube.Zone (TimeZone)
import Foreign.Storable (Storable)
import GHC.Generics (Generic)

-- |
-- Days since Unix epoch.
newtype UnixDate (cal :: Calendar) (tz :: TimeZone) = UnixDate {getDate :: Int32}
   deriving (Eq, Generic, NFData, Ord, Storable)

-- |
-- Seconds since Unix epoch.
newtype UnixTime (cal :: Calendar) (tz :: TimeZone) = UnixTime {getTime :: Int64}
   deriving (Eq, Generic, NFData, Ord, Storable)
