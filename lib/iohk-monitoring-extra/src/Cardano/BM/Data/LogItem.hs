{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Representation of traced messages as log objects.
module Cardano.BM.Data.LogItem
    ( LogObject (..)
    , LOMeta (..)
    , mkLOMeta
    , LOContent (..)
    , Measurable (..)
    , LoggerName
    , PrivacyAnnotation (..)
    ) where

import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Control.Concurrent
    ( myThreadId
    )
import Control.Monad.IO.Class
    ( MonadIO
    , liftIO
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , object
    , withObject
    , withText
    , (.:)
    , (.=)
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Text
    ( Text
    , pack
    , stripPrefix
    )
import Data.Time.Clock
    ( UTCTime
    , getCurrentTime
    )
import Data.Word
    ( Word64
    )
import GHC.Generics
    ( Generic
    )
import Prelude

-- | A 'LoggerName' is currently a dotted 'Text' context path.
type LoggerName = Text

-- | Meta data for a 'LogObject'.
data LOMeta = LOMeta
    { tstamp :: {-# UNPACK #-} !UTCTime
    , tid :: {-# UNPACK #-} !Text
    , hostname :: {-# UNPACK #-} !Text
    , severity :: !Severity
    , privacy :: !PrivacyAnnotation
    }

instance ToJSON LOMeta where
    toJSON (LOMeta tstamp0 tid0 hn0 sev0 priv0) =
        object
            [ "tstamp" .= tstamp0
            , "tid" .= tid0
            , "hostname" .= hn0
            , "severity" .= show sev0
            , "privacy" .= show priv0
            ]

instance FromJSON LOMeta where
    parseJSON = withObject "LOMeta" $ \v ->
        LOMeta
            <$> v .: "tstamp"
            <*> v .: "tid"
            <*> v .: "hostname"
            <*> v .: "severity"
            <*> v .: "privacy"

instance Show LOMeta where
    show (LOMeta tstamp1 tid1 hn1 _sev1 _priv1) =
        "LOMeta@"
            ++ show tstamp1
            ++ " tid="
            ++ show tid1
            ++ if not $ null $ show hn1 then " on " ++ show hn1 else ""

instance Eq LOMeta where
    (==) (LOMeta tstamp1 tid1 hn1 sev1 priv1) (LOMeta tstamp2 tid2 hn2 sev2 priv2) =
        tstamp1 == tstamp2
            && tid1 == tid2
            && hn1 == hn2
            && sev1 == sev2
            && priv1 == priv2

-- | Create 'LOMeta' with the current time and thread id.
mkLOMeta :: MonadIO m => Severity -> PrivacyAnnotation -> m LOMeta
mkLOMeta sev priv =
    LOMeta
        <$> liftIO getCurrentTime
        <*> (cleantid <$> liftIO myThreadId)
        <*> pure ""
        <*> pure sev
        <*> pure priv
  where
    cleantid threadid =
        fromMaybe tidText $ stripPrefix "ThreadId " tidText
      where
        tidText = pack $ show threadid

-- | Privacy annotation of a traced item.
data PrivacyAnnotation
    = -- | confidential information - handle with care
      Confidential
    | -- | indifferent - can be public.
      Public
    deriving (Show, Eq, Ord, Enum, Bounded)

instance FromJSON PrivacyAnnotation where
    parseJSON = withText "PrivacyAnnotation" $ \case
        "Confidential" -> pure Confidential
        "Public" -> pure Public
        _ -> fail "unknown PrivacyAnnotation"

instance ToJSON PrivacyAnnotation where
    toJSON = toJSON . show

-- | Outcome of a trace: a named context, metadata, and payload.
data LogObject a = LogObject
    { loName :: LoggerName
    , loMeta :: !LOMeta
    , loContent :: !(LOContent a)
    }
    deriving (Show, Eq)

instance Functor LogObject where
    fmap f (LogObject nm me loc) = LogObject nm me (fmap f loc)

instance ToJSON a => ToJSON (LogObject a) where
    toJSON (LogObject loname lometa locontent) =
        object
            [ "loname" .= loname
            , "lometa" .= lometa
            , "locontent" .= locontent
            ]

-- | Payload of a 'LogObject'.
data LOContent a
    = LogMessage a
    | LogError !Text
    | LogValue !Text !Measurable
    deriving (Show, Eq)

instance Functor LOContent where
    fmap f = \case
        LogMessage msg -> LogMessage (f msg)
        LogError a -> LogError a
        LogValue n v -> LogValue n v

instance ToJSON a => ToJSON (LOContent a) where
    toJSON = \case
        LogMessage m ->
            object ["kind" .= ("LogMessage" :: Text), "message" .= toJSON m]
        LogError m ->
            object ["kind" .= ("LogError" :: Text), "message" .= m]
        LogValue n v ->
            object ["kind" .= ("LogValue" :: Text), "name" .= n, "value" .= v]

-- | A value that can be traced as a metric.
data Measurable
    = Microseconds {-# UNPACK #-} !Word64
    | Nanoseconds {-# UNPACK #-} !Word64
    | Seconds {-# UNPACK #-} !Word64
    | Bytes {-# UNPACK #-} !Word64
    | PureD !Double
    | PureI !Integer
    | Severity Severity
    deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)
