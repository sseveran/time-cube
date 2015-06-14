{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall            #-}

-- |
-- Module      : Data.Time.Cube.Lens
-- Copyright   : Copyright (c) 2015, Alpha Heavy Industries, Inc. All rights reserved.
-- License     : BSD3
-- Maintainer  : Enzo Haussecker <ehaussecker@alphaheavy.com>
-- Stability   : Experimental
-- Portability : Portable
--
-- A lens-based interface to date and time data structures.
module Data.Time.Cube.Lens (

 -- ** Lenses
       d_year
     , d_mon
     , d_mday
     , d_wday

     , t_hour
     , t_min
     , t_sec

     , dt_year
     , dt_mon
     , dt_mday
     , dt_wday
     , dt_hour
     , dt_min
     , dt_sec

     , ld_year
     , ld_mon
     , ld_mday
     , ld_wday
     , ld_zone

     , lt_hour
     , lt_min
     , lt_sec
     , lt_zone

     , ldt_year
     , ldt_mon
     , ldt_mday
     , ldt_wday
     , ldt_hour
     , ldt_min
     , ldt_sec
     , ldt_zone

     , ps_year
     , ps_mon
     , ps_mday
     , ps_wday
     , ps_hour
     , ps_min
     , ps_sec
     , ps_frac
     , ps_ampm
     , ps_zone

     ) where

import Control.Lens.TH (makeLenses)
import Data.Time.Cube.Base
import Data.Time.Cube.Parser

makeLenses ''DateStruct
makeLenses ''TimeStruct
makeLenses ''DateTimeStruct
makeLenses ''LocalDateStruct
makeLenses ''LocalTimeStruct
makeLenses ''LocalDateTimeStruct
makeLenses ''ParserState