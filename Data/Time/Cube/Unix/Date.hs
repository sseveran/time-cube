{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall                       #-}

-- |
-- Module      : Data.Time.Cube.Unix.Date
-- Copyright   : Copyright (c) 2014, Alpha Heavy Industries, Inc.
-- License     : BSD3
-- Maintainer  : Enzo Haussecker <enzo@ucsd.edu>
-- Stability   : Stable
-- Portability : Portable
--
-- The Unix date.
module Data.Time.Cube.Unix.Date (

 -- ** Type
       UnixDate(..)

 -- ** Current
     , getCurrentUnixDate

     ) where

import Control.DeepSeq (NFData)
import Data.Int (Int32)
import Data.Time.Cube.Base (Calendar)
import Data.Time.Cube.Unix (C'timeval(..), getTimeOfDay)
import Foreign.Storable (Storable)
import GHC.Generics (Generic)

-- |
-- Days since Unix epoch.
newtype UnixDate (cal :: Calendar) = UnixDate {getBase :: Int32}
   deriving (Eq, Generic, NFData, Ord, Storable)

-- |
-- Get the current Unix date from the system clock.
getCurrentUnixDate :: IO (UnixDate (cal :: Calendar))
getCurrentUnixDate =
   getTimeOfDay >>= \ (C'timeval base _) ->
   return $! UnixDate . fromIntegral $ base `div` 86400
