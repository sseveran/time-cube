{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall         #-}

-- |
-- Module      : Data.Time.Cube.Human
-- Copyright   : Copyright (c) 2014, Alpha Heavy Industries, Inc. All rights reserved.
-- License     : Apache License, Version 2.0
-- Maintainer  : Enzo Haussecker <enzo@ucsd.edu>
-- Stability   : Stable
-- Portability : Portable
--
-- A class for human-readable timestamps.
module Data.Time.Cube.Human (

 -- ** Class
       Human(..)

     ) where

class Human x where

   -- |
   -- Define the human-readable components of a timestamp.
   data Components x :: *

   -- |
   -- Compose a timestamp from human-readable components.
   pack :: Components x -> x

   -- |
   -- Decompose a timestamp into human-readable components.
   unpack :: x -> Components x
