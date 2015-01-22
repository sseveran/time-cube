{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS -Wall                      #-}

-- |
-- Module      : Data.Time.Cube.Zones
-- License     : BSD3
-- Maintainer  : ehaussecker@alphaheavy.com
-- Stability   : Experimental
-- Portability : GHC 7.8.* on Unix
--
-- Dependently typed time zones and related utilities.
module Data.Time.Cube.Zones (

 -- ** Time Zones
       TimeZone(..)

 -- ** Parameterizations
     , Universal
     , Unix
     , Offset
     , SomeOffset(..)
     , Olson

 -- ** Abbreviations
     , Abbreviate(..)

     ) where

import Control.Applicative  ((<|>), (*>))
import Control.DeepSeq      (NFData(..))
import Control.Monad        (replicateM)
import Data.Attoparsec.Text (char, digit, parseOnly)
import Data.Int             (Int16)
import Data.Proxy           (Proxy(..))
import Data.Text            (Text, pack)
import Foreign.Ptr          (castPtr)
import Foreign.Storable     (Storable(..))
import GHC.Generics         (Generic)
import GHC.TypeLits
import GHC.TypeLits.SigNat
import Text.Printf          (printf)

-- |
-- A uniform standard for time.
data family TimeZone :: * -> *

data Universal :: *

data Unix :: *

data Offset :: SigNat -> *

data SomeOffset = forall signat . KnownSigNat signat => SomeOffset (Proxy signat)

data family Olson :: Symbol -> *

data instance TimeZone Universal =
     UTC
     deriving (Eq, Generic, Show)

data instance TimeZone Unix =
     UnixTime
     deriving (Eq, Generic, Show)

data instance TimeZone (Offset signat) =
     TimeZoneOffset
     deriving (Eq, Generic, Show)

data instance TimeZone SomeOffset =
     forall signat . KnownSigNat signat =>
     SomeTimeZoneOffset (Proxy signat)

data instance TimeZone (Olson olson) =
     TimeZoneOlson
     deriving (Eq, Generic, Show)

instance Eq (SomeOffset) where

  SomeOffset x == SomeOffset y = sigNatVal x == sigNatVal y

instance Eq (TimeZone SomeOffset) where

  SomeTimeZoneOffset x == SomeTimeZoneOffset y = sigNatVal x == sigNatVal y

deriving instance Show (SomeOffset)
deriving instance Show (TimeZone SomeOffset)

instance Storable (TimeZone SomeOffset) where

  sizeOf _  = 2
  alignment = sizeOf
  peekElemOff ptr n = do
    val <- peekElemOff (castPtr ptr) n :: IO Int16
    case someSigNatVal $ toInteger val
      of SomeSigNat proxy -> return $! SomeTimeZoneOffset proxy
  pokeElemOff ptr n (SomeTimeZoneOffset proxy) =
    pokeElemOff (castPtr ptr) n val
    where val = fromInteger $ sigNatVal proxy :: Int16

class Abbreviate tz where

  -- |
  -- Get the abbreviation text for the given time zone.
  abbreviate :: tz -> Text

  -- |
  -- Get the time zone for the given abbreviation text.
  unabbreviate :: Text -> Either String tz

instance Abbreviate (TimeZone Universal) where

  abbreviate UTC = "UTC"

  -- |
  -- Unabbreviate a universal time zone.
  unabbreviate = \ case
    "UTC"  -> Right UTC
    _      -> Left "unabbreviate{TimeZone Universal}: mismatch"

instance KnownSigNat signat => Abbreviate (TimeZone (Offset signat)) where

  -- |
  -- Abbreviate a time zone offset that is known at runtime.
  abbreviate TimeZoneOffset =
    pack $ sign : hours ++ minutes
    where
      sign    = if signat < 0 then '-' else '+'
      hours   = printf "%02d" $ div nat 60
      minutes = printf "%02d" $ mod nat 60
      nat     = abs signat
      signat  = sigNatVal (Proxy :: Proxy signat)

  -- |
  -- Unabbreviate a time zone offset that is known at runtime.
  unabbreviate =
    parseOnly $ do
      sign    <- plus <|> minus
      hours   <- replicateM 2 digit
      minutes <- replicateM 2 digit
      let proxy  = Proxy :: Proxy signat
          signat = sign $ read hours * 60 + read minutes
      if  sigNatVal proxy /= signat
      then fail "unabbreviate{TimeZone (Offset signat)}: mismatch"
      else return TimeZoneOffset
           where plus  = char '+' *> return id
                 minus = char '-' *> return negate

instance Abbreviate (TimeZone SomeOffset) where

  -- |
  -- Abbreviate a time zone offset that is unknown at runtime.
  abbreviate (SomeTimeZoneOffset proxy) =
    pack $ sign : hours ++ minutes
    where
      sign    = if signat < 0 then '-' else '+'
      hours   = printf "%02d" $ div nat 60
      minutes = printf "%02d" $ mod nat 60
      nat     = abs signat
      signat  = sigNatVal proxy

  -- |
  -- Unabbreviate a time zone offset that is unknown at runtime.
  unabbreviate =
    parseOnly $ do
      sign    <- plus <|> minus
      hours   <- replicateM 2 digit
      minutes <- replicateM 2 digit
      let signat = sign $ read hours * 60 + read minutes
      case someSigNatVal signat
        of SomeSigNat proxy -> return $! promoteSigNat proxy SomeTimeZoneOffset
           where plus  = char '+' *> return id
                 minus = char '-' *> return negate

instance NFData (TimeZone (Universal    )) where rnf _ = ()
instance NFData (TimeZone (Unix         )) where rnf _ = ()
instance NFData (TimeZone (Offset signat)) where rnf _ = ()
instance NFData (TimeZone (SomeOffset   )) where rnf _ = ()
instance NFData (TimeZone (Olson olson  )) where rnf _ = ()