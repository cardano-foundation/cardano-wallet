{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Severity levels for log messages and traces.
module Cardano.Wallet.Tracing.Data.Severity
    ( Severity (..)
    ) where

import Data.Aeson
    ( FromJSON (..)
    , ToJSON
    , withText
    )
import GHC.Generics
    ( Generic
    )
import Prelude

data Severity
    = Debug
    | Info
    | Notice
    | Warning
    | Error
    | Critical
    | Alert
    | Emergency
    deriving (Show, Eq, Ord, Bounded, Enum, Generic, ToJSON, Read)

-- | 'Severity' is a lower semilattice, and thus a monoid.
instance Semigroup Severity where
    Debug <> _ = Debug
    _ <> Debug = Debug
    Info <> _ = Info
    _ <> Info = Info
    Notice <> _ = Notice
    _ <> Notice = Notice
    Warning <> _ = Warning
    _ <> Warning = Warning
    Error <> _ = Error
    _ <> Error = Error
    Critical <> _ = Critical
    _ <> Critical = Critical
    Alert <> _ = Alert
    _ <> Alert = Alert
    Emergency <> Emergency = Emergency

instance Monoid Severity where
    mempty = Emergency

instance FromJSON Severity where
    parseJSON = withText "severity" $ \case
        "Debug" -> pure Debug
        "Info" -> pure Info
        "Notice" -> pure Notice
        "Warning" -> pure Warning
        "Error" -> pure Error
        "Critical" -> pure Critical
        "Alert" -> pure Alert
        "Emergency" -> pure Emergency
        _ -> pure Info -- catch all
