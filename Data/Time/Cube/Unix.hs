{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall                       #-}

-- |
-- Module      : Data.Time.Cube.Unix
-- Copyright   : Copyright (c) 2014, Alpha Heavy Industries, Inc.
-- License     : BSD3
-- Maintainer  : Enzo Haussecker <enzo@ucsd.edu>
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
import Foreign.Storable (Storable)
import GHC.Generics (Generic)

-- |
-- Days since Unix epoch.
newtype UnixDate (cal :: Calendar) = UnixDate {getDate :: Int32}
   deriving (Eq, Generic, NFData, Ord, Storable)

-- |
-- Seconds since Unix epoch.
newtype UnixTime (cal :: Calendar) = UnixTime {getTime :: Int64}
   deriving (Eq, Generic, NFData, Ord, Storable)
