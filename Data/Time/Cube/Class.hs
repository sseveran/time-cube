{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall                  #-}

-- |
-- Module      : Data.Time.Cube.Class
-- Copyright   : Copyright (c) 2014, Alpha Heavy Industries, Inc. All rights reserved.
-- License     : Apache License, Version 2.0
-- Maintainer  : Enzo Haussecker <enzo@ucsd.edu>
-- Stability   : Stable
-- Portability : Portable
--
-- Basic timestamp type classes.
module Data.Time.Cube.Class (

 -- ** Classes
       Human(..)
     , Math(..)

     ) where

class Human x where

   -- |
   -- Define the human-readable components of a timestamp.
   type Components x :: *

   -- |
   -- Pack a timestamp from human-readable components.
   pack :: Components x -> x

   -- |
   -- Unpack a timestamp into human-readable components.
   unpack :: x -> Components x

class Math x c where

   -- |
   -- Compute the duration between two timestamps.
   duration :: x -> x -> c

   -- |
   -- Add time to a timestamp.
   plus :: x -> c -> x