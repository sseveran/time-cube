{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS -Wall           #-}

-- |
-- Module      : Data.Time.Cube.Human
-- Copyright   : Copyright (c) 2014, Alpha Heavy Industries, Inc. All rights reserved.
-- License     : Apache License, Version 2.0
-- Maintainer  : Enzo Haussecker <enzo@ucsd.edu>
-- Stability   : Stable
-- Portability : Portable
--
-- Classes for human-readable dates and time.
module Data.Time.Cube.Human where

import Data.Time.Cube.Base (Calendar)
import Data.Time.Cube.Struct

class HumanDate x where

   -- |
   -- Compose a timestamp from date components.
   fromDateStruct :: DateStruct (cal :: Calendar) -> x

   -- |
   -- Decompose a timestamp into date components.
   toDateStruct :: x -> DateStruct (cal :: Calendar)

class HumanTime x where

   -- |
   -- Compose a timestamp from time components.
   fromTimeStruct :: TimeStruct -> x

   -- |
   -- Decompose a timestamp into time components.
   toTimeStruct :: x -> TimeStruct

class (HumanDate x, HumanTime x) => HumanDateTime x where

   -- |
   -- Compose a timestamp from date and time components.
   fromDateTimeStruct :: DateTimeStruct (cal :: Calendar) -> x

   -- |
   -- Decompose a timestamp into date and time components.
   toDateTimeStruct :: x -> DateTimeStruct (cal :: Calendar)

class HumanLocalDate x where

   -- |
   -- Compose a timestamp from local date components.
   fromLocalDateStruct :: LocalDateStruct (cal :: Calendar) -> x

   -- |
   -- Decompose a timestamp into local date components.
   toLocalDateStruct :: x -> LocalDateStruct (cal :: Calendar)

class HumanLocalTime x where

   -- |
   -- Compose a timestamp from local time components.
   fromLocalTimeStruct :: LocalTimeStruct -> x

   -- |
   -- Decompose a timestamp into local time components.
   toLocalTimeStruct :: x -> LocalTimeStruct

class (HumanLocalDate x, HumanLocalTime x) => HumanLocalDateTime x where

   -- |
   -- Compose a timestamp from local date and time components.
   fromLocalDateTimeStruct :: LocalDateTimeStruct (cal :: Calendar) -> x

   -- |
   -- Decompose a timestamp into local date and time components.
   toLocalDateTimeStruct :: x -> LocalDateTimeStruct (cal :: Calendar)
