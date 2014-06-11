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
-- Unix date and timestamps.
module Data.Time.Cube.Unix (

 -- ** Types
       UnixDate(..)
     , UnixDateTime(..)

     ) where

import Control.DeepSeq (NFData)
import Data.Int (Int32, Int64)
import Data.Time.Cube.Base (Calendar)
import Foreign.Storable (Storable)
import GHC.Generics (Generic)

-- |
-- Days since Unix epoch.
newtype UnixDate (cal :: Calendar) = UnixDate {getUnixDate :: Int32}
   deriving (Eq, Generic, NFData, Ord, Storable)

-- |
-- Seconds since Unix epoch.
newtype UnixDateTime (cal :: Calendar) = UnixDateTime {getUnixDateTime :: Int64}
   deriving (Eq, Generic, NFData, Ord, Storable)
