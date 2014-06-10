{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS -Wall               #-}
{-# OPTIONS -fno-warn-orphans   #-}

-- |
-- Module      : Data.Time.Cube.Calendar.Gregorian
-- Copyright   : Copyright (c) 2014, Alpha Heavy Industries, Inc.
-- License     : BSD3
-- Maintainer  : Enzo Haussecker <enzo@ucsd.edu>
-- Stability   : Stable
-- Portability : Portable
--
-- The Gregorian calendar system.
module Data.Time.Cube.Calendar.Gregorian (

 -- ** Chronologies
       Era(..)

 -- ** Components
     , Month(..)
     , DayOfWeek(..)

     ) where

import Data.Time.Cube.Base (Calendar(Gregorian), Era, Month, DayOfWeek)
import GHC.Generics (Generic)

data instance Era Gregorian =
     BeforeChrist
   | AnnoDomini

data instance Month Gregorian =
     January
   | February
   | March
   | April
   | May
   | June
   | July
   | August
   | September
   | October
   | November
   | December

data instance DayOfWeek Gregorian =
     Sunday
   | Monday
   | Tuesday
   | Wednesday
   | Thursday
   | Friday
   | Saturday

deriving instance Eq (Era Gregorian)
deriving instance Eq (Month Gregorian)
deriving instance Eq (DayOfWeek Gregorian)

deriving instance Enum (Era Gregorian)
deriving instance Enum (Month Gregorian)
deriving instance Enum (DayOfWeek Gregorian)

deriving instance Generic (Era Gregorian)
deriving instance Generic (Month Gregorian)
deriving instance Generic (DayOfWeek Gregorian)

deriving instance Ord (Era Gregorian)
deriving instance Ord (Month Gregorian)
deriving instance Ord (DayOfWeek Gregorian)

deriving instance Read (Era Gregorian)
deriving instance Read (Month Gregorian)
deriving instance Read (DayOfWeek Gregorian)

deriving instance Show (Era Gregorian)
deriving instance Show (Month Gregorian)
deriving instance Show (DayOfWeek Gregorian)
