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
-- Maintainer  : Enzo Haussecker
-- Stability   : Stable
-- Portability : Portable
--
-- Dependently typed time zones and related utilities.
module Data.Time.Cube.Zones (

 -- ** Time Zones
       TimeZone(..)

 -- ** Geographies
     , Universal
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
-- A uniform standard for time with geographic parametrization.
data family TimeZone :: * -> *

data Universal :: *

data Offset :: SigNat -> *

data SomeOffset = forall signat . KnownSigNat signat => SomeOffset (Proxy signat)

instance Eq SomeOffset where

   SomeOffset x == SomeOffset y = sigNatVal x == sigNatVal y

deriving instance Show SomeOffset

data family Olson :: Symbol -> *

data instance TimeZone Universal =
     CoordinatedUniversalTime
     deriving (Eq, Generic, Show)

data instance TimeZone (Offset signat) =
     TimeZoneOffset
     deriving (Eq, Generic, Show)

data instance TimeZone SomeOffset =
     forall signat . KnownSigNat signat =>
     SomeTimeZoneOffset (Proxy signat)

instance Eq (TimeZone SomeOffset) where

   SomeTimeZoneOffset x == SomeTimeZoneOffset y = sigNatVal x == sigNatVal y

deriving instance Show (TimeZone SomeOffset)

instance Storable (TimeZone SomeOffset) where

   sizeOf  _ = 2
   alignment = sizeOf
   peekElemOff ptr n = do
     val <- peekElemOff (castPtr ptr) n :: IO Int16
     case someSigNatVal $ toInteger val
       of SomeSigNat proxy -> return $! SomeTimeZoneOffset proxy
   pokeElemOff ptr n (SomeTimeZoneOffset proxy) =
     pokeElemOff (castPtr ptr) n val
     where val = fromInteger $ sigNatVal proxy :: Int16

data instance TimeZone (Olson olson) =
    TimeZoneOlson
    deriving (Eq, Generic, Show)

class Abbreviate tz where

   -- |
   -- Get the abbreviation text for the given time zone.
   abbreviate :: tz -> Text

   -- |
   -- Get the time zone for the given abbreviation text.
   unabbreviate :: Text -> Either String tz

instance Abbreviate (TimeZone Universal) where

   abbreviate CoordinatedUniversalTime = "UTC"

   unabbreviate = \ case
     "UTC" -> Right CoordinatedUniversalTime
     _     -> Left "unabbreviate{TimeZone Universal}: unknown"

instance Abbreviate (TimeZone SomeOffset) where

   abbreviate (SomeTimeZoneOffset proxy) =
     pack $ sign : hours ++ minutes
     where
       sign    = if signat < 0 then '-' else '+'
       hours   = printf "%02d" $ div nat 60
       minutes = printf "%02d" $ mod nat 60
       nat     = abs signat
       signat  = sigNatVal proxy

   unabbreviate = parseOnly $ do
     sign    <- plus <|> minus
     hours   <- replicateM 2 digit
     minutes <- replicateM 2 digit
     let signat = sign $ read hours * 60 + read minutes
     case someSigNatVal signat
       of SomeSigNat proxy -> return $! promoteSigNat proxy SomeTimeZoneOffset
          where plus  = char '+' *> return id
                minus = char '-' *> return negate

instance NFData (TimeZone (Universal    )) where rnf _ = ()
instance NFData (TimeZone (Offset signat)) where rnf _ = ()
instance NFData (TimeZone (SomeOffset   )) where rnf _ = ()
instance NFData (TimeZone (Olson olson  )) where rnf _ = ()

















{-

data instance TimeZone (Olson "America/Los_Angeles") =
     PacificStandardTime
   | PacificDaylightTime
     deriving (Eq, Generic, Show)

data instance TimeZone (Olson "America/New_York") =
     EasternStandardTime
   | EasternDaylightTime
     deriving (Eq, Generic, Show)
-}








{-
data instance TimeZone (Olson "America/Los_Angeles") =
     PacificStandardTime
   | PacificDaylightTime
     deriving (Eq, Generic, Show)


data instance TimeZone (Olson "America/Denver") =
     MountainStandardTime
   | MountainDaylightTime
     deriving (Eq, Generic, Show)

data instance TimeZone (Olson "America/New_York") =
     EasternStandardTime
   | EasternDaylightTime
     deriving (Eq, Generic, Show)

data instance TimeZone (Olson "Asia/Kabul") =
     AfghanistanTime
     deriving (Eq, Generic, Show)
-}




























{-



-}
---------------------------------------------------------
------------------------ SigNats ------------------------
---------------------------------------------------------














--------------------------------------------------------
---------------------------SigNat S-----------------------------
--------------------------------------------------------






















{-














data instance TimeZone (Olson "Asia/Kabul") =
     AfghanistanTime
     deriving (Eq, Generic, Show)







class Unabbreviate tz where




instance KnownSigNat int => Unabbreviate (TimeZone (Offset int)) where







-- promoteOffset :: forall int a . KnownSigNat int => proxy int -> (Proxy int -> a) -> a
f
promoteOffset :: TimeZone (Offset int) -> SomeOffset -> TimeZone (Offset int)
promoteOffset a _ = a






 =
     SomeOffset2
     deriving (Eq, Generic, Show)



data instance TimeZone KnownOffset =
     KnownOffset
     deriving (Eq, Generic, Show)




-- |
-- Any geographic region characterized by its offset in minutes from a prime meridian.
data Offset :: SigNat -> *

-- |
-- Any geographic region where local clocks have stayed consistent since 1970.
data family Olson :: Symbol -> *



data instance TimeZone (Offset int) =
     Offset
     deriving (Eq, Generic, Show)




instance KnownSigNat int => Abbreviate (Offset int) where








data instance (Olson "America/Anchorage") = Anchorage
     deriving (Eq, Generic)

data instance (Olson "America/Chicago") = Chicago
     deriving (Eq, Generic)

data instance (Olson "America/Denver") = Denver
     deriving (Eq, Generic)

data instance (Olson "America/Los_Angeles") = LosAngeles
     deriving (Eq, Generic)

data instance (Olson "America/New_York") = NewYork
     deriving (Eq, Generic)

data instance (Olson "Asia/Kabul") = Kabul
     deriving (Eq, Generic)

instance Show (Olson "America/Anchorage") where

    show Anchorage = "America/Anchorage"

instance Show (Olson "America/Chicago") where

    show Chicago = "America/Chicago"

instance Show (Olson "America/Denver") where

    show Denver = "America/Denver"

instance Show (Olson "America/Los_Angeles") where

    show LosAngeles = "America/Los_Angeles"

instance Show (Olson "America/New_York") where

    show NewYork = "America/New_York"

instance Show (Olson "Asia/Kabul") where

    show Kabul = "Asia/Kabul"







instance Abbreviate (Olson "America/Anchorage") where

   abbreviate = \ case
     AlaskaStandardTime -> "AKST"
     AlaskaDaylightTime -> "AKDT"

   unabbreviate = \ case
     "AKST" -> Right AlaskaStandardTime
     "AKDT" -> Right AlaskaDaylightTime
     txt    -> Left $ "unabbreviate{TimeZone (Olson \"America/Anchorage\")}: " ++ show txt

instance Abbreviate (Olson "America/Chicago") where

   abbreviate = \ case
     CentralStandardTime -> "CST"
     CentralDaylightTime -> "CDT"

   unabbreviate = \ case
     "CST" -> Right CentralStandardTime
     "CDT" -> Right CentralDaylightTime
     txt   -> Left $ "unabbreviate{TimeZone (Olson \"America/Chicago\")}: " ++ show txt

instance Abbreviate (Olson "America/Denver") where

   abbreviate = \ case
     MountainStandardTime -> "MST"
     MountainDaylightTime -> "MDT"

   unabbreviate = \ case
     "MST" -> Right MountainStandardTime
     "MDT" -> Right MountainDaylightTime
     txt   -> Left $ "unabbreviate{TimeZone (Olson \"America/Denver\")}: " ++ show txt

instance Abbreviate (Olson "America/Los_Angeles") where

   abbreviate = \ case
     PacificStandardTime -> "PST"
     PacificDaylightTime -> "PDT"

   unabbreviate = \ case
     "PST" -> Right PacificStandardTime
     "PDT" -> Right PacificDaylightTime
     txt   -> Left $ "unabbreviate{TimeZone (Olson \"America/Los_Angeles\")}: " ++ show txt

instance Abbreviate (Olson "America/New_York") where

   abbreviate = \ case
     EasternStandardTime -> "EST"
     EasternDaylightTime -> "EDT"

   unabbreviate = \ case
     "EST" -> Right EasternStandardTime
     "EDT" -> Right EasternDaylightTime
     txt   -> Left $ "unabbreviate{TimeZone (Olson \"America/New_York\")}: " ++ show txt

instance Abbreviate (Olson "Asia/Kabul") where

   abbreviate AfghanistanTime = "AFT"

   unabbreviate = \ case
     "AFT" -> Right AfghanistanTime
     txt   -> Left $ "unabbreviate{TimeZone (Olson \"Asia/Kabul\")}: " ++ show txt

-- |
-- Convert from one time zone to another.
class Convert a b where

   convert :: TimeZone a -> Either String (TimeZone b)

instance Convert Universal (Offset (Plus 0)) where

   convert CoordinatedUniversalTime = Right Offset

instance Convert Universal (Offset (Minus 0)) where

   convert CoordinatedUniversalTime = Right Offset

instance Convert (Olson "America/Anchorage") (Offset (Minus 540)) where

   convert AlaskaStandardTime = Right Offset
   convert tz                 = Left $ "convert{Olson \"America/Anchorage\", Offset (Minus 540)}: " ++ show tz

instance Convert (Olson "America/Anchorage") (Offset (Minus 480)) where

   convert AlaskaDaylightTime = Right Offset
   convert tz                 = Left $ "convert{Olson \"America/Anchorage\", Offset (Minus 480)}: " ++ show tz

instance Convert (Olson "America/Chicago") (Offset (Minus 360)) where

   convert CentralStandardTime = Right Offset
   convert tz                  = Left $ "convert{Olson \"America/Chicago\", Offset (Minus 360)}: " ++ show tz

instance Convert (Olson "America/Chicago") (Offset (Minus 300)) where

   convert CentralDaylightTime = Right Offset
   convert tz                  = Left $ "convert{Olson \"America/Chicago\", Offset (Minus 300)}: " ++ show tz

instance Convert (Olson "America/Denver") (Offset (Minus 420)) where

   convert MountainStandardTime = Right Offset
   convert tz                   = Left $ "convert{Olson \"America/Denver\", Offset (Minus 420)}: " ++ show tz

instance Convert (Olson "America/Denver") (Offset (Minus 360)) where

   convert MountainDaylightTime = Right Offset
   convert tz                   = Left $ "convert{Olson \"America/Denver\", Offset (Minus 360)}: " ++ show tz

instance Convert (Olson "America/Los_Angeles") (Offset (Minus 480)) where

   convert PacificStandardTime = Right Offset
   convert tz                  = Left $ "convert{Olson \"America/Los_Angeles\", Offset (Minus 480)}: " ++ show tz

instance Convert (Olson "America/Los_Angeles") (Offset (Minus 420)) where

   convert PacificDaylightTime = Right Offset
   convert tz                  = Left $ "convert{Olson \"America/Los_Angeles\", Offset (Minus 420)}: " ++ show tz

instance Convert (Olson "America/New_York") (Offset (Minus 300)) where

   convert EasternStandardTime = Right Offset
   convert tz                  = Left $ "convert{Olson \"America/New_York\", Offset (Minus 300)}: " ++ show tz

instance Convert (Olson "America/New_York") (Offset (Minus 240)) where

   convert EasternDaylightTime = Right Offset
   convert tz                  = Left $ "convert{Olson \"America/New_York\", Offset (Minus 240)}: " ++ show tz

instance Convert (Olson "Asia/Kabul") (Offset (Plus 270)) where

   convert AfghanistanTime = Right Offset











-}



