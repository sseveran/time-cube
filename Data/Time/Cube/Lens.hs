{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall            #-}

-- |
-- Module      : Data.Time.Cube.Lens
-- License     : BSD3
-- Maintainer  : Enzo Haussecker
-- Stability   : Stable
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

     ) where

import Control.Lens.TH (makeLenses)
import Data.Time.Cube.Base

makeLenses ''DateStruct
makeLenses ''TimeStruct
makeLenses ''DateTimeStruct
makeLenses ''LocalDateStruct
makeLenses ''LocalTimeStruct
makeLenses ''LocalDateTimeStruct
