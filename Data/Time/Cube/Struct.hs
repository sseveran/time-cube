{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall                 #-}

-- |
-- Module      : Data.Time.Cube.Struct
-- Copyright   : Copyright (c) 2014, Alpha Heavy Industries, Inc.
-- License     : BSD3
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

 -- ** Local Structs 
     , LocalDateStruct(..)
     , LocalTimeStruct(..)
     , LocalDateTimeStruct(..)

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
       } deriving (Generic)

-- |
-- A struct with time components.
data TimeStruct =
     TimeStruct
       { _t_hour :: {-# UNPACK #-} !Hour
       , _t_min  :: {-# UNPACK #-} !Minute
       , _t_sec  :: {-# UNPACK #-} !Double
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
       } deriving (Generic)

-- |
-- A struct with date and location components.
data LocalDateStruct (cal :: Calendar) =
     LocalDateStruct
       { _ld_year :: {-# UNPACK #-} !(Year)
       , _ld_mon  ::                !(Month     (cal :: Calendar))
       , _ld_mday :: {-# UNPACK #-} !(Day)
       , _ld_wday ::                !(DayOfWeek (cal :: Calendar))
       , _ld_zone ::                !(TimeZone)
       } deriving (Generic)

-- |
-- A struct with time and location components.
data LocalTimeStruct =
     LocalTimeStruct
       { _lt_hour :: {-# UNPACK #-} !Hour
       , _lt_min  :: {-# UNPACK #-} !Minute
       , _lt_sec  :: {-# UNPACK #-} !Double
       , _lt_zone ::                !TimeZone
       } deriving (Eq, Generic, Ord, Show)

-- |
-- A struct with date, time and location components.
data LocalDateTimeStruct (cal :: Calendar) =
     LocalDateTimeStruct
       { _ldt_year :: {-# UNPACK #-} !(Year)
       , _ldt_mon  ::                !(Month     (cal :: Calendar))
       , _ldt_mday :: {-# UNPACK #-} !(Day)
       , _ldt_wday ::                !(DayOfWeek (cal :: Calendar))
       , _ldt_hour :: {-# UNPACK #-} !(Hour)
       , _ldt_min  :: {-# UNPACK #-} !(Minute)
       , _ldt_sec  :: {-# UNPACK #-} !(Double)
       , _ldt_zone ::                !(TimeZone)
       } deriving (Generic)

deriving instance (Eq   (Month               (cal :: Calendar)),
                   Eq   (DayOfWeek           (cal :: Calendar))) =>
                   Eq   (DateStruct          (cal :: Calendar))

deriving instance (Eq   (Month               (cal :: Calendar)),
                   Eq   (DayOfWeek           (cal :: Calendar))) =>
                   Eq   (DateTimeStruct      (cal :: Calendar))

deriving instance (Eq   (Month               (cal :: Calendar)),
                   Eq   (DayOfWeek           (cal :: Calendar))) =>
                   Eq   (LocalDateStruct     (cal :: Calendar))

deriving instance (Eq   (Month               (cal :: Calendar)),
                   Eq   (DayOfWeek           (cal :: Calendar))) =>
                   Eq   (LocalDateTimeStruct (cal :: Calendar))

deriving instance (Ord  (Month               (cal :: Calendar)),
                   Ord  (DayOfWeek           (cal :: Calendar))) =>
                   Ord  (DateStruct          (cal :: Calendar))

deriving instance (Ord  (Month               (cal :: Calendar)),
                   Ord  (DayOfWeek           (cal :: Calendar))) =>
                   Ord  (DateTimeStruct      (cal :: Calendar))

deriving instance (Ord  (Month               (cal :: Calendar)),
                   Ord  (DayOfWeek           (cal :: Calendar))) =>
                   Ord  (LocalDateStruct     (cal :: Calendar))

deriving instance (Ord  (Month               (cal :: Calendar)),
                   Ord  (DayOfWeek           (cal :: Calendar))) =>
                   Ord  (LocalDateTimeStruct (cal :: Calendar))

deriving instance (Show (Month               (cal :: Calendar)),
                   Show (DayOfWeek           (cal :: Calendar))) =>
                   Show (DateStruct          (cal :: Calendar))

deriving instance (Show (Month               (cal :: Calendar)),
                   Show (DayOfWeek           (cal :: Calendar))) =>
                   Show (DateTimeStruct      (cal :: Calendar))

deriving instance (Show (Month               (cal :: Calendar)),
                   Show (DayOfWeek           (cal :: Calendar))) =>
                   Show (LocalDateStruct     (cal :: Calendar))

deriving instance (Show (Month               (cal :: Calendar)),
                   Show (DayOfWeek           (cal :: Calendar))) =>
                   Show (LocalDateTimeStruct (cal :: Calendar))
