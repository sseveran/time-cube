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
-- Copyright   : Copyright (c) 2015, Alpha Heavy Industries, Inc. All rights reserved.
-- License     : BSD3
-- Maintainer  : Enzo Haussecker <ehaussecker@alphaheavy.com>
-- Stability   : Experimental
-- Portability : Portable
--
-- This module provides time zone data types and related parameterizations.
module Data.Time.Cube.Zones (

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
     , promoteTimeZone

     ) where

import Control.Applicative        ((<|>), (*>))
import Control.DeepSeq            (NFData(..))
import Control.Monad              (replicateM)
import Data.Attoparsec.Text       (char, digit, parseOnly)
import Data.ByteString.Char8 as B (unpack)
import Data.Int                   (Int16)
import Data.Proxy                 (Proxy(..))
import Data.Text as T             (Text, pack, unpack)
import Data.Time.Zones            (TZ, diffForAbbr)
import Data.Time.Zones.DB         (TZLabel, toTZName)
import Foreign.Ptr                (castPtr)
import Foreign.Storable           (Storable(..))
import GHC.Generics               (Generic)
import GHC.TypeLits
import GHC.TypeLits.SigNat
import Text.Printf                (printf)

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
    where val = fromInteger . normalizeOffset $ sigNatVal proxy :: Int16

class Abbreviate tz where

  -- |
  -- Get the abbreviation text for the given time zone.
  abbreviate :: tz -> Text

  -- |
  -- Get the time zone for the given abbreviation text.
  unabbreviate :: Maybe (TZLabel, TZ) -> Text -> Either String tz

instance Abbreviate (TimeZone None) where

  -- |
  -- Abbreviate a time zone with no parameterization.
  abbreviate NoTimeZone = ""

  -- |
  -- Unabbreviate a time zone with no parameterization.
  unabbreviate _ = \ case
    ""   -> Right NoTimeZone
    text -> Left $ "unabbreviate{TimeZone None}: unmatched text: " ++ T.unpack text

instance Abbreviate (TimeZone UTC) where

  -- |
  -- Abbreviate the UTC time zone.
  abbreviate UTC = "UTC"

  -- |
  -- Unabbreviate the UTC time zone.
  unabbreviate _ = \ case
    "UTC" -> Right UTC
    text  -> Left $ "unabbreviate{TimeZone UTC}: unmatched text: " ++ T.unpack text

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
  unabbreviate _ =
    parseOnly $ do
      sign    <- plus <|> minus
      hours   <- replicateM 2 digit
      minutes <- replicateM 2 digit
      let proxy  = Proxy :: Proxy signat
          signat = normalizeOffset . sign $ read hours * 60 + read minutes
      if  normalizeOffset (sigNatVal proxy) == signat
      then return TimeZoneOffset
      else fail $ "unabbreviate{TimeZone (Offset signat)}: unmatched type signature: " ++ show signat
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
  unabbreviate _ =
    parseOnly $ do
      sign    <- plus <|> minus
      hours   <- replicateM 2 digit
      minutes <- replicateM 2 digit
      let signat = normalizeOffset . sign $ read hours * 60 + read minutes
      case someSigNatVal signat
        of SomeSigNat proxy -> return $! promoteOffset proxy SomeTimeZoneOffset
           where plus  = char '+' *> return id
                 minus = char '-' *> return negate

instance KnownSymbol symbol => Abbreviate (TimeZone (Olson region symbol signat)) where

  -- |
  -- Abbreviate an Olson time zone that is known at compile time.
  abbreviate TimeZoneOlson = pack $ symbolVal (Proxy :: Proxy symbol)

  -- |
  -- Unabbreviate an Olson time zone that is known at compile time.
  unabbreviate _ text =
    if T.unpack text == symbolVal (Proxy :: Proxy symbol)
    then Right TimeZoneOlson
    else Left $ "unabbreviate{TimeZone (Olson region symbol signat)}: unmatched type signature: " ++ T.unpack text

instance Abbreviate (TimeZone SomeOlson) where

  -- |
  -- Abbreviate an Olson time zone that is unknown at compile time.
  --
  -- > λ> :set -XDataKinds
  -- > λ> :module + Data.Proxy GHC.TypeLits.SigNat
  -- > λ> let abbreviation = Proxy :: Proxy "CEST"
  -- > λ> let offset = Proxy :: Proxy (Plus 120)
  -- > λ> abbreviate $ SomeTimeZoneOlson "Europe/Paris" abbreviation offset
  -- > "CEST"
  --
  abbreviate (SomeTimeZoneOlson _ proxy _) = pack $ symbolVal proxy

  -- |
  -- Unabbreviate an Olson time zone that is unknown at compile time.
  --
  -- > λ> :set -XOverloadedStrings -XTupleSections
  -- > λ> :module + Control.Applicative Data.Time.Zones Data.Time.Zones.DB 
  -- > λ> tzdata <- Just . (Europe__Paris, ) <$> loadTZFromDB "Europe/Paris"
  -- > λ> unabbreviate tzdata "CEST" :: Either String (TimeZone SomeOlson)
  -- > Right (SomeTimeZoneOlson "Europe/Paris" Proxy Proxy)
  --
  unabbreviate mval text =
    case mval
      of Nothing -> Left "unabbreviate{TimeZone SomeOlson}: no Olson data to match text"
         Just (label, tz) ->
           let symbol = T.unpack text
           in case diffForAbbr tz symbol
                of Nothing -> Left "unabbreviate{TimeZone SomeOlson}: unmatched text"
                   Just diff ->
                     case someSymbolVal symbol
                       of SomeSymbol proxy1 ->
                            case someSigNatVal . toInteger $ div diff 60
                              of SomeSigNat proxy2 ->
                                   let region = B.unpack $ toTZName label
                                   in Right . promoteOlson proxy1 proxy2 $ SomeTimeZoneOlson region

instance NFData (TimeZone (None)) where rnf _ = ()
instance NFData (TimeZone (UTC)) where rnf _ = ()
instance NFData (TimeZone (Offset signat)) where rnf _ = ()
instance NFData (TimeZone (SomeOffset)) where rnf _ = ()
instance NFData (TimeZone (Olson region symbol signat)) where rnf _ = ()
instance NFData (TimeZone (SomeOlson)) where rnf _ = ()

-- |
-- Map a time zone offset to the interval (-720, 720].
normalizeOffset :: Integer -> Integer
normalizeOffset = pivot . flip mod 1440
  where pivot n = if -720 < n && n <= 720 then n else n - signum n * 1440

-- |
-- Promote a time zone offset to the type level.
promoteOffset
  :: forall proxy signat a . KnownSigNat signat
  => proxy signat
  -> (Proxy signat -> a)
  -> a
promoteOffset = promoteSigNat

-- |
-- Promote a time zone abbreviation and offset to the type level.
promoteOlson
  :: forall proxy1 proxy2 symbol signat a . (KnownSymbol symbol, KnownSigNat signat)
  => proxy1 symbol
  -> proxy2 signat
  -> (Proxy symbol -> Proxy signat -> a)
  -> a
promoteOlson _ _ f = f Proxy Proxy

-- |
-- Promote a time zone to the type level.
promoteTimeZone
  :: forall proxy param a . proxy (TimeZone param)
  -> (Proxy (TimeZone param) -> a)
  -> a
promoteTimeZone _ f = f Proxy