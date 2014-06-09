{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall                       #-}

-- |
-- Module      : Data.Time.Cube.Unix.Date
-- Copyright   : Copyright (c) 2014, Alpha Heavy Industries, Inc. All rights reserved.
-- License     : Apache License, Version 2.0
-- Maintainer  : Enzo Haussecker <enzo@ucsd.edu>
-- Stability   : Stable
-- Portability : Portable
--
-- The Unix datestamp.
module Data.Time.Cube.Unix.Date (

 -- ** Type
       UnixDate(..)

     ) where

import Control.DeepSeq (NFData)
import Data.Int (Int32)
import Data.Time.Cube.Base (Calendar)
import GHC.Generics (Generic)

-- |
-- Days since Unix epoch. 
newtype UnixDate (cal :: Calendar) = UnixDate {getBase :: Int32}
    deriving (Eq, Generic, NFData, Ord)
