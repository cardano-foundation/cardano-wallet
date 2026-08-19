{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.Api.Types.Dapp
    ( ApiDappCapabilities (..)
    , ApiDappBackendBuild (..)
    , ApiDappNetwork (..)
    , ApiDappCapability (..)
    , makeApiDappCapabilities
    ) where

import Cardano.Wallet.Api.Lib.Options
    ( strictRecordTypeOptions
    )
import Cardano.Wallet.Api.Types.Era
    ( ApiEra (..)
    )
import Control.Monad
    ( unless
    , when
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , genericParseJSON
    , genericToJSON
    )
import Data.Aeson.Types
    ( Parser
    )
import Data.Char
    ( isAsciiLower
    , isDigit
    , isHexDigit
    )
import Data.List
    ( find
    )
import Data.Text
    ( Text
    )
import GHC.Generics
    ( Generic
    )
import Prelude

import qualified Cardano.Wallet.Read as Read
import qualified Data.Set as Set
import qualified Data.Text as T

data ApiDappCapabilities = ApiDappCapabilities
    { apiVersion :: !Integer
    , backendBuild :: !ApiDappBackendBuild
    , network :: !ApiDappNetwork
    , capabilities :: ![ApiDappCapability]
    }
    deriving (Eq, Generic, Show)

data ApiDappBackendBuild = ApiDappBackendBuild
    { version :: !Text
    , sourceRevision :: !Text
    }
    deriving (Eq, Generic, Show)

data ApiDappNetwork = ApiDappNetwork
    { networkId :: !Integer
    , networkMagic :: !Integer
    , genesisHash :: !Text
    , currentEra :: !ApiEra
    }
    deriving (Eq, Generic, Show)

data ApiDappCapability = ApiDappCapability
    { name :: !Text
    , revision :: !Integer
    , availableEras :: ![ApiEra]
    }
    deriving (Eq, Generic, Show)

instance FromJSON ApiDappCapabilities where
    parseJSON value = do
        result@ApiDappCapabilities{apiVersion, capabilities} <-
            genericParseJSON strictRecordTypeOptions value
        unless (apiVersion == 1) $ fail "api_version must be 1"
        rejectDuplicates "capability names" (name <$> capabilities)
        let requireCapability requiredName =
                case find ((== requiredName) . name) capabilities of
                    Nothing -> fail $ "missing capability: " <> T.unpack requiredName
                    Just ApiDappCapability{revision, availableEras} -> do
                        unless (revision == 1)
                            $ fail "required capability revision must be 1"
                        unless (availableEras == [ApiConway])
                            $ fail "required capability must be Conway-only"
        mapM_ requireCapability requiredCapabilities
        pure result

instance ToJSON ApiDappCapabilities where
    toJSON = genericToJSON strictRecordTypeOptions

instance FromJSON ApiDappBackendBuild where
    parseJSON value = do
        result@ApiDappBackendBuild{sourceRevision} <-
            genericParseJSON strictRecordTypeOptions value
        unless (isLowerHexOfLength 40 sourceRevision)
            $ fail "source_revision must be 40 lowercase hexadecimal characters"
        pure result

instance ToJSON ApiDappBackendBuild where
    toJSON = genericToJSON strictRecordTypeOptions

instance FromJSON ApiDappNetwork where
    parseJSON value = do
        result@ApiDappNetwork{networkId, networkMagic, genesisHash} <-
            genericParseJSON strictRecordTypeOptions value
        unless (networkId == 0 || networkId == 1)
            $ fail "network_id must be 0 or 1"
        unless (networkMagic >= 0 && networkMagic <= maxWord32)
            $ fail "network_magic must fit Word32"
        unless (isLowerHexOfLength 64 genesisHash)
            $ fail "genesis_hash must be 64 lowercase hexadecimal characters"
        pure result

instance ToJSON ApiDappNetwork where
    toJSON = genericToJSON strictRecordTypeOptions

instance FromJSON ApiDappCapability where
    parseJSON value = do
        result@ApiDappCapability{name, revision, availableEras} <-
            genericParseJSON strictRecordTypeOptions value
        unless (isKebabCase name)
            $ fail "capability name must be lowercase kebab case"
        unless (revision > 0 && revision <= maxSafeInteger)
            $ fail "capability revision must be a positive safe integer"
        when (null availableEras) $ fail "available_eras must not be empty"
        rejectDuplicates "available eras" availableEras
        pure result

instance ToJSON ApiDappCapability where
    toJSON = genericToJSON strictRecordTypeOptions

-- The HTTP route stays unavailable until task-209. This constructor freezes the
-- complete response and refuses every era except Conway before ApiEra conversion.
makeApiDappCapabilities
    :: ApiDappBackendBuild
    -> Integer
    -> Integer
    -> Text
    -> Read.EraValue Read.Era
    -> Maybe ApiDappCapabilities
makeApiDappCapabilities backendBuild networkId networkMagic genesisHash era
    | not
        (validProducerInputs backendBuild networkId networkMagic genesisHash) =
        Nothing
    | Read.EraValue Read.Conway <- era =
        Just
            ApiDappCapabilities
                { apiVersion = 1
                , backendBuild
                , network =
                    ApiDappNetwork
                        { networkId
                        , networkMagic
                        , genesisHash
                        , currentEra = ApiConway
                        }
                , capabilities = makeCapability <$> requiredCapabilities
                }
    | otherwise = Nothing
  where
    makeCapability name =
        ApiDappCapability
            { name
            , revision = 1
            , availableEras = [ApiConway]
            }

validProducerInputs
    :: ApiDappBackendBuild -> Integer -> Integer -> Text -> Bool
validProducerInputs ApiDappBackendBuild{sourceRevision} networkId networkMagic genesisHash =
    isLowerHexOfLength 40 sourceRevision
        && (networkId == 0 || networkId == 1)
        && networkMagic >= 0
        && networkMagic <= maxWord32
        && isLowerHexOfLength 64 genesisHash

requiredCapabilities :: [Text]
requiredCapabilities =
    [ "transaction-context"
    , "reviewed-context-signing"
    , "cip8-cip95"
    , "durable-wallet-submit"
    ]

maxSafeInteger :: Integer
maxSafeInteger = 9007199254740991

maxWord32 :: Integer
maxWord32 = 4294967295

isLowerHexOfLength :: Int -> Text -> Bool
isLowerHexOfLength expected value =
    T.length value == expected
        && T.all (\c -> isHexDigit c && not (c >= 'A' && c <= 'F')) value

isKebabCase :: Text -> Bool
isKebabCase value =
    not (T.null value)
        && all validSegment (T.splitOn "-" value)
  where
    validSegment segment =
        not (T.null segment)
            && T.all (\c -> isAsciiLower c || isDigit c) segment

rejectDuplicates :: Ord a => String -> [a] -> Parser ()
rejectDuplicates label values =
    unless (Set.size (Set.fromList values) == length values)
        $ fail
        $ "duplicate " <> label
