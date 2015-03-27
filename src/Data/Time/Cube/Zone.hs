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
-- Module      : Data.Time.Cube.Zone
-- License     : BSD3
-- Maintainer  : ehaussecker@alphaheavy.com
-- Stability   : Experimental
-- Portability : GHC 7.8.* on Unix
--
-- Dependently typed time zones and related utilities.
module Data.Time.Cube.Zone (

 -- ** Time Zones
       TimeZone(..)

 -- ** Parameterizations
     , None
     , UTC
     , Offset
     , SomeOffset(..)
     , Olson
     , SomeOlson(..)

 -- ** Abbreviations
     , Abbreviate(..)

 -- ** Utilities
     , normalizeOffset
     , promoteOffset
     , promoteOlson

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

data None :: *

data UTC :: *

data Offset :: SigNat -> *

data SomeOffset =
     forall signat . KnownSigNat signat =>
     SomeOffset (Proxy signat)

data family Olson :: Symbol -> Symbol -> SigNat -> *

data SomeOlson =
     forall symbol signat . (KnownSymbol symbol, KnownSigNat signat) =>
     SomeOlson String (Proxy symbol) (Proxy signat)

data instance TimeZone None =
     NoTimeZone
     deriving (Eq, Generic, Show)

data instance TimeZone UTC =
     UTC
     deriving (Eq, Generic, Show)

data instance TimeZone (Offset signat) =
     TimeZoneOffset
     deriving (Eq, Generic, Show)

data instance TimeZone SomeOffset =
     forall signat . KnownSigNat signat =>
     SomeTimeZoneOffset (Proxy signat)

data instance TimeZone (Olson region symbol signat) =
     TimeZoneOlson
     deriving (Eq, Generic, Show)

data instance TimeZone SomeOlson =
     forall symbol signat . (KnownSymbol symbol, KnownSigNat signat) =>
     SomeTimeZoneOlson String (Proxy symbol) (Proxy signat)

instance Eq (SomeOffset) where
       (==) (SomeOffset x1)
            (SomeOffset y1) = sigNatVal x1 ==
                              sigNatVal y1

instance Eq (SomeOlson) where
       (==) (SomeOlson x1 x2 x3)
            (SomeOlson y1 y2 y3) = x1 ==
                                   y1 && symbolVal x2 ==
                                         symbolVal y2 && sigNatVal x3 ==
                                                         sigNatVal y3

instance Eq (TimeZone SomeOffset) where
       (==) (SomeTimeZoneOffset x1)
            (SomeTimeZoneOffset y1) = sigNatVal x1 ==
                                      sigNatVal y1

instance Eq (TimeZone SomeOlson) where
       (==) (SomeTimeZoneOlson x1 x2 x3)
            (SomeTimeZoneOlson y1 y2 y3) = x1 ==
                                           y1 && symbolVal x2 ==
                                                 symbolVal y2 && sigNatVal x3 ==
                                                                 sigNatVal y3

deriving instance Show (SomeOffset)
deriving instance Show (SomeOlson)
deriving instance Show (TimeZone SomeOffset)
deriving instance Show (TimeZone SomeOlson)

instance Storable (TimeZone SomeOffset) where

  -- |
  -- Size of a time zone offset that is unknown at compile time.
  sizeOf = const 2

  -- |
  -- Alignment of a time zone offset that is unknown at compile time.
  alignment = sizeOf

  -- |
  -- Read a time zone offset that is unknown at compile time from an array.
  peekElemOff ptr n = do
    val <- peekElemOff (castPtr ptr) n :: IO Int16
    case someSigNatVal $ toInteger val
      of SomeSigNat proxy -> return $! SomeTimeZoneOffset proxy

  -- |
  -- Write a time zone offset that is unknown at compile time to an array.
  pokeElemOff ptr n (SomeTimeZoneOffset proxy) =
    pokeElemOff (castPtr ptr) n val
    where val = normalizeOffset $ sigNatVal proxy :: Int16

class Abbreviate tz where

  -- |
  -- Get the abbreviation text for the given time zone.
  abbreviate :: tz -> Text

  -- |
  -- Get the time zone for the given abbreviation text.
  unabbreviate :: Text -> Either String tz

instance Abbreviate (TimeZone None) where

  -- |
  -- Abbreviate a time zone with no parameterization.
  abbreviate NoTimeZone = ""

  -- |
  -- Unabbreviate a time zone with no parameterization.
  unabbreviate = \ case
    "" -> Right NoTimeZone
    _  -> Left $ "unabbreviate{TimeZone None}: unmatched text"

instance Abbreviate (TimeZone UTC) where

  -- |
  -- Abbreviate the UTC time zone.
  abbreviate UTC = "UTC"

  -- |
  -- Unabbreviate the UTC time zone.
  unabbreviate = \ case
    "UTC" -> Right UTC
    _     -> Left $ "unabbreviate{TimeZone UTC}: unmatched text"

instance KnownSigNat signat => Abbreviate (TimeZone (Offset signat)) where

  -- |
  -- Abbreviate a time zone offset that is known at compile time.
  abbreviate TimeZoneOffset =
    pack $ sign : hours ++ minutes
    where
      sign    = if signat < 0 then '-' else '+'
      hours   = printf "%02d" $ div nat 60
      minutes = printf "%02d" $ mod nat 60
      nat     = abs signat
      signat  = normalizeOffset $ sigNatVal (Proxy :: Proxy signat)

  -- |
  -- Unabbreviate a time zone offset that is known at compile time.
  unabbreviate =
    parseOnly $ do
      sign    <- plus <|> minus
      hours   <- replicateM 2 digit
      minutes <- replicateM 2 digit
      let proxy  = Proxy :: Proxy signat
          signat = sign $ read hours * 60 + read minutes
      if  sigNatVal proxy /= signat
      then fail "unabbreviate{TimeZone (Offset signat)}: unmatched type signature"
      else return TimeZoneOffset
           where plus  = char '+' *> return id
                 minus = char '-' *> return negate

instance Abbreviate (TimeZone SomeOffset) where

  -- |
  -- Abbreviate a time zone offset that is unknown at compile time.
  abbreviate (SomeTimeZoneOffset proxy) =
    pack $ sign : hours ++ minutes
    where
      sign    = if signat < 0 then '-' else '+'
      hours   = printf "%02d" $ div nat 60
      minutes = printf "%02d" $ mod nat 60
      nat     = abs signat
      signat  = normalizeOffset $ sigNatVal proxy

  -- |
  -- Unabbreviate a time zone offset that is unknown at compile time.
  unabbreviate =
    parseOnly $ do
      sign    <- plus <|> minus
      hours   <- replicateM 2 digit
      minutes <- replicateM 2 digit
      let signat = sign $ read hours * 60 + read minutes
      case someSigNatVal signat
        of SomeSigNat proxy -> return $! promoteOffset proxy SomeTimeZoneOffset
           where plus  = char '+' *> return id
                 minus = char '-' *> return negate

instance NFData (TimeZone (None)) where rnf _ = ()
instance NFData (TimeZone (UTC)) where rnf _ = ()
instance NFData (TimeZone (Offset signat)) where rnf _ = ()
instance NFData (TimeZone (SomeOffset)) where rnf _ = ()
instance NFData (TimeZone (Olson region symbol signat)) where rnf _ = ()
instance NFData (TimeZone (SomeOlson)) where rnf _ = ()

-- |
-- Map a time zone offset to the interval [-720, 720].
normalizeOffset :: Integer -> Int16
normalizeOffset = fromInteger . pivot . flip mod 1440
  where pivot n = if abs n <= 720 then n else n - signum n * 1440

-- |
-- Promote a time zone offset to the type level.
promoteOffset :: forall proxy signat offset . KnownSigNat signat => proxy signat -> (Proxy signat -> offset) -> offset
promoteOffset = promoteSigNat

-- |
-- Promote a time zone abbreviation and offset to the type level.
promoteOlson
  :: forall proxy1 proxy2 symbol signat olson . (KnownSymbol symbol, KnownSigNat signat)
  => proxy1 symbol
  -> proxy2 signat
  -> (Proxy symbol -> Proxy signat -> olson)
  -> olson
promoteOlson _ _ f = f Proxy Proxy