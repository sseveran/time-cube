{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -Wall                  #-}

-- |
-- Module      : Data.Time.Cube.Math
-- Copyright   : Copyright (c) 2014, Alpha Heavy Industries, Inc. All rights reserved.
-- License     : Apache License, Version 2.0
-- Maintainer  : Enzo Haussecker <enzo@ucsd.edu>
-- Stability   : Stable
-- Portability : Portable
--
-- A class for time math.
module Data.Time.Cube.Math (

 -- ** Class
       Math(..)

     ) where

class Math x c where

   -- | Add a timestamp with a component.
   plus :: x -> c -> x
