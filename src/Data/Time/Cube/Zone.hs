{-# LANGUAGE BangPatterns              #-}
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
module Data.Time.Cube.Zone (

 -- ** Time Zones
       TimeZone(..)

 -- ** Parameterizations
     , Unix
     , UTC
     , Offset
     , SomeOffset(..)
     , Olson
     , SomeOlson(..)

 -- ** Abbreviations
     , Abbreviate(..)

 -- ** Utilities
     , normalizeOffset
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

data Unix :: *

data UTC :: *

data Offset :: SigNat -> *

data SomeOffset =
     forall signat . KnownSigNat signat =>
     SomeOffset (Proxy signat)

data family Olson :: Symbol -> Symbol -> SigNat -> *

data SomeOlson =
     forall symbol signat . (KnownSymbol symbol, KnownSigNat signat) =>
     SomeOlson String (Proxy symbol) (Proxy signat)

data instance TimeZone Unix =
     Unix
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
  -- Size of an unknown time zone offset.
  sizeOf = const 2

  -- |
  -- Alignment of an unknown time zone offset.
  alignment = sizeOf

  -- |
  -- Read an unknown time zone offset from an array.
  peekElemOff ptr n = do
    val <- peekElemOff (castPtr ptr) n :: IO Int16
    case someSigNatVal $ toInteger val
      of SomeSigNat proxy -> return $! SomeTimeZoneOffset proxy

  -- |
  -- Write an unknown time zone offset to an array.
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

instance Abbreviate (TimeZone Unix) where

  -- |
  -- Abbreviate the Unix time zone.
  abbreviate Unix = ""

  -- |
  -- Unabbreviate a Unix time zone.
  unabbreviate = \ case
    "" -> Right Unix
    _  -> Left $ "unabbreviate{TimeZone Unix}: unmatched text"

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
      then fail "unabbreviate{TimeZone (Offset signat)}: unmatched type"
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

instance NFData (TimeZone (Unix)) where rnf _ = ()
instance NFData (TimeZone (UTC)) where rnf _ = ()
instance NFData (TimeZone (Offset signat)) where rnf _ = ()
instance NFData (TimeZone (SomeOffset)) where rnf _ = ()
instance NFData (TimeZone (Olson region symbol signat)) where rnf _ = ()
instance NFData (TimeZone (SomeOlson)) where rnf _ = ()

-- |
-- Map an arbitrary time zone offset to the interval [-720, 720].
normalizeOffset :: Integer -> Int16
normalizeOffset = fromInteger . reduce
  where reduce !n = if abs n <= 720 then n else reduce $ n - signum n * 1440

-- |
-- Promote Olson data to the type level.
promoteOlson
  :: forall proxy1 proxy2 symbol signat olson . (KnownSymbol symbol, KnownSigNat signat)
  => proxy1 symbol
  -> proxy2 signat
  -> (Proxy symbol -> Proxy signat -> olson)
  -> olson
promoteOlson _ _ f = f Proxy Proxy