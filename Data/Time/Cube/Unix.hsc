{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall                  #-}
{-# OPTIONS -fno-warn-orphans      #-}

-- |
-- Module      : Data.Time.Cube.Unix
-- Copyright   : Copyright (c) 2014, Alpha Heavy Industries, Inc.
-- License     : BSD3
-- Maintainer  : Enzo Haussecker <enzo@ucsd.edu>
-- Stability   : Stable
-- Portability : Unix
--
-- Bindings to the GNU C time library.
module Data.Time.Cube.Unix where

import Data.Time.Cube.Class (Human(..))
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CLong, CTime(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (FunPtr, Ptr, nullPtr, plusPtr)
import Foreign.Storable (Storable(..))

#include <bindings.dsl.h>
#include <time.h>
#include <sys/time.h>

#starttype struct tm
#field tm_sec    , CInt
#field tm_min    , CInt
#field tm_hour   , CInt
#field tm_mday   , CInt
#field tm_mon    , CInt
#field tm_year   , CInt
#field tm_wday   , CInt
#field tm_yday   , CInt
#field tm_isdst  , CInt
#field tm_gmtoff , CLong
#field tm_zone   , CString
#stoptype

#starttype struct timeval
#field tv_sec  , CLong
#field tv_usec , CLong
#stoptype

#ccall timegm , Ptr <tm> -> IO CTime
#ccall gmtime_r , Ptr CTime -> Ptr <tm> -> IO (Ptr <tm>)
#ccall gettimeofday , Ptr <timeval> -> Ptr () -> IO CInt

instance Human CTime where
    type Components CTime = C'tm
    pack   = unsafeLocalState . flip with c'timegm
    unpack = unsafeLocalState . flip with go where
      go x = alloca $ \ ptr -> c'gmtime_r x ptr >>= peek

-- |
-- Get the current time from the system clock.
getTimeOfDay :: IO C'timeval
getTimeOfDay = with (C'timeval 0 0) $ \ ptr -> c'gettimeofday ptr nullPtr >>= getResult ptr
    where getResult ptr 0 = peek ptr
          getResult _ err = error $ "getTimeOfDay: " ++ show err
