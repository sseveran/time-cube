{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall                 #-}

-- |
-- Module      : Data.Time.Cube.Struct
-- Copyright   : Copyright (c) 2014, Alpha Heavy Industries, Inc. All rights reserved.
-- License     : Apache License, Version 2.0
-- Maintainer  : Enzo Haussecker <enzo@ucsd.edu>
-- Stability   : Stable
-- Portability : Portable
--
-- Struct data types for dates and time.
module Data.Time.Cube.Struct (

 -- ** Structs
       DateStruct(..)
     , TimeStruct(..)
     , DateTimeStruct(..)

     ) where

import Data.Time.Cube.Base
import Data.Time.Cube.Zone (TimeZone)
import GHC.Generics (Generic)

-- |
-- A struct with date components.
data DateStruct (cal :: Calendar) =
     DateStruct
       { _d_year :: {-# UNPACK #-} !(Year)
       , _d_mon  ::                !(Month     (cal :: Calendar))
       , _d_mday :: {-# UNPACK #-} !(Day)
       , _d_wday ::                !(DayOfWeek (cal :: Calendar))
       , _d_zone ::                !(TimeZone)
       } deriving (Generic)

-- |
-- A struct with time components.
data TimeStruct =
     TimeStruct
       { _t_hour :: {-# UNPACK #-} !Hour
       , _t_min  :: {-# UNPACK #-} !Minute
       , _t_sec  :: {-# UNPACK #-} !Double
       , _t_zone ::                !TimeZone
       } deriving (Eq, Generic, Ord, Show)

-- |
-- A struct with date and time components.
data DateTimeStruct (cal :: Calendar) =
     DateTimeStruct
       { _dt_year :: {-# UNPACK #-} !(Year)
       , _dt_mon  ::                !(Month     (cal :: Calendar))
       , _dt_mday :: {-# UNPACK #-} !(Day)
       , _dt_wday ::                !(DayOfWeek (cal :: Calendar))
       , _dt_hour :: {-# UNPACK #-} !(Hour)
       , _dt_min  :: {-# UNPACK #-} !(Minute)
       , _dt_sec  :: {-# UNPACK #-} !(Double)
       , _dt_zone ::                !(TimeZone)
       } deriving (Generic)

deriving instance (Eq   (Month          (cal :: Calendar)),
                   Eq   (DayOfWeek      (cal :: Calendar))) =>
                   Eq   (DateStruct     (cal :: Calendar))

deriving instance (Eq   (Month          (cal :: Calendar)),
                   Eq   (DayOfWeek      (cal :: Calendar))) =>
                   Eq   (DateTimeStruct (cal :: Calendar))

deriving instance (Ord  (Month          (cal :: Calendar)),
                   Ord  (DayOfWeek      (cal :: Calendar))) =>
                   Ord  (DateStruct     (cal :: Calendar))

deriving instance (Ord  (Month          (cal :: Calendar)),
                   Ord  (DayOfWeek      (cal :: Calendar))) =>
                   Ord  (DateTimeStruct (cal :: Calendar))

deriving instance (Show (Month          (cal :: Calendar)),
                   Show (DayOfWeek      (cal :: Calendar))) =>
                   Show (DateStruct     (cal :: Calendar))

deriving instance (Show (Month          (cal :: Calendar)),
                   Show (DayOfWeek      (cal :: Calendar))) =>
                   Show (DateTimeStruct (cal :: Calendar))
