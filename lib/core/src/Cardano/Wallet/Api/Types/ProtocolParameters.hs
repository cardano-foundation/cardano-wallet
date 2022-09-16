{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Api.Types.ProtocolParameters
    where

import Prelude

import Data.Aeson
    ( ToJSON (..), ToJSONKey (..), FromJSON (..), FromJSONKey (..) )
import Data.Aeson.Encode.Pretty
    ( encodePretty )
import Data.Function
    ( (&) )
import Data.Functor.Contravariant
    ( contramap )
import Data.OpenApi
    ( ToSchema (..), toSchema )
import Data.Typeable
    ( Typeable )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word64 )
import GHC.Generics
    ( Generic, Rep )
import Data.Map.Strict
    ( Map )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Gen )

import qualified Data.Aeson as Aeson
import qualified Data.OpenApi as OpenApi
import qualified Data.OpenApi.Internal.Schema as OpenApi
import qualified Cardano.Api.Gen as CardanoApi
import qualified Cardano.Api.Shelley as CardanoApi
import qualified Data.Aeson.Yaml as AesonYaml
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as Map
import qualified Data.Ratio as Ratio

--------------------------------------------------------------------------------
-- ApiCostModel
--------------------------------------------------------------------------------

newtype ApiCostModel = ApiCostModel (Map Text Integer)
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON, ToJSON, ToSchema) via DefaultSchema ApiCostModel

mkApiCostModel :: CardanoApi.CostModel -> ApiCostModel
mkApiCostModel (CardanoApi.CostModel m) = ApiCostModel m

--------------------------------------------------------------------------------
-- ApiEpochNo
--------------------------------------------------------------------------------

newtype ApiEpochNo = ApiEpochNo Word64
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON, ToJSON, ToSchema) via DefaultSchema ApiEpochNo

mkApiEpochNo :: CardanoApi.EpochNo -> ApiEpochNo
mkApiEpochNo (CardanoApi.EpochNo n) = ApiEpochNo n

--------------------------------------------------------------------------------
-- ApiExecutionUnits
--------------------------------------------------------------------------------

data ApiExecutionUnits = ApiExecutionUnits
    { executionMemory
        :: Natural
    , executionSteps
        :: Natural
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON, ToJSON, ToSchema) via DefaultSchema ApiExecutionUnits

mkApiExecutionUnits :: CardanoApi.ExecutionUnits -> ApiExecutionUnits
mkApiExecutionUnits CardanoApi.ExecutionUnits {..} = ApiExecutionUnits {..}

--------------------------------------------------------------------------------
-- ApiExecutionUnitPrices
--------------------------------------------------------------------------------

data ApiExecutionUnitPrices = ApiExecutionUnitPrices
    { priceExecutionMemory
        :: ApiRational
    , priceExecutionSteps
        :: ApiRational
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON, ToJSON, ToSchema)
        via DefaultSchema ApiExecutionUnitPrices

mkApiExecutionUnitPrices
    :: CardanoApi.ExecutionUnitPrices -> ApiExecutionUnitPrices
mkApiExecutionUnitPrices
    CardanoApi.ExecutionUnitPrices
        { priceExecutionMemory
        , priceExecutionSteps
        } = ApiExecutionUnitPrices
            { priceExecutionMemory =
                mkApiRational priceExecutionMemory
            , priceExecutionSteps =
                mkApiRational priceExecutionSteps
            }

--------------------------------------------------------------------------------
-- ApiLovelace
--------------------------------------------------------------------------------

newtype ApiLovelace = ApiLovelace Integer
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON, ToJSON, ToSchema) via DefaultSchema ApiLovelace

mkApiLovelace :: CardanoApi.Lovelace -> ApiLovelace
mkApiLovelace (CardanoApi.Lovelace n) = ApiLovelace n

--------------------------------------------------------------------------------
-- ApiPlutusScriptVersion
--------------------------------------------------------------------------------

data ApiPlutusScriptVersion
    = PlutusScriptV1
    | PlutusScriptV2
    deriving stock (Eq, Generic, Ord, Show)
    deriving (FromJSON, FromJSONKey, ToJSON, ToJSONKey, ToSchema)
        via DefaultSchema ApiPlutusScriptVersion

mkApiPlutusScriptVersion
    :: CardanoApi.AnyPlutusScriptVersion -> ApiPlutusScriptVersion
mkApiPlutusScriptVersion = \case
    (CardanoApi.AnyPlutusScriptVersion CardanoApi.PlutusScriptV1) ->
        PlutusScriptV1
    (CardanoApi.AnyPlutusScriptVersion CardanoApi.PlutusScriptV2) ->
        PlutusScriptV1

--------------------------------------------------------------------------------
-- ApiPraosNonce
--------------------------------------------------------------------------------

newtype ApiPraosNonce = ApiPraosNonce String
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON, ToJSON, ToSchema) via DefaultSchema ApiPraosNonce

mkApiPraosNonce :: CardanoApi.PraosNonce -> ApiPraosNonce
mkApiPraosNonce = ApiPraosNonce . show

--------------------------------------------------------------------------------
-- ApiProtocolVersion
--------------------------------------------------------------------------------

data ApiProtocolVersion = ApiProtocolVersion
    { major
        :: Natural
    , minor
        :: Natural
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON, ToJSON, ToSchema)
        via DefaultSchema ApiProtocolVersion

mkApiProtocolVersion :: (Natural, Natural) -> ApiProtocolVersion
mkApiProtocolVersion (major, minor) = ApiProtocolVersion {major, minor}

--------------------------------------------------------------------------------
-- ApiRatio
--------------------------------------------------------------------------------

data ApiRational = ApiRational
    { numerator
        :: Integer
    , denominator
        :: Integer
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON, ToJSON, ToSchema) via DefaultSchema ApiRational

mkApiRational :: Rational -> ApiRational
mkApiRational r = ApiRational
    { numerator =
        Ratio.numerator r
    , denominator =
        Ratio.denominator r
    }

--------------------------------------------------------------------------------
-- ApiProtocolParameters
--------------------------------------------------------------------------------

data ApiProtocolParameters = ApiProtocolParameters
    { protocolVersion
        :: ApiProtocolVersion
    , collateralPercentage
        :: Maybe Natural
    , costModels
        :: Maybe (Map ApiPlutusScriptVersion ApiCostModel)
    , decentralization
        :: Maybe ApiRational
    , extraPraosEntropy
        :: Maybe ApiPraosNonce
    , maxBlockBodySize
        :: Maybe Natural
    , maxBlockExUnits
        :: Maybe ApiExecutionUnits
    , maxBlockHeaderSize
        :: Maybe Natural
    , maxCollateralInputs
        :: Maybe Natural
    , maxTxExUnits
        :: Maybe ApiExecutionUnits
    , maxTxSize
        :: Maybe Natural
    , maxValueSize
        :: Maybe Natural
    , minPoolCost
        :: Maybe ApiLovelace
    , minUTxOValue
        :: Maybe ApiLovelace
    , monetaryExpansion
        :: Maybe ApiRational
    , poolPledgeInfluence
        :: Maybe ApiRational
    , poolRetireMaxEpoch
        :: Maybe ApiEpochNo
    , prices
        :: Maybe ApiExecutionUnitPrices
    , stakeAddressDeposit
        :: Maybe ApiLovelace
    , stakePoolDeposit
        :: Maybe ApiLovelace
    , stakePoolTargetNum
        :: Maybe Natural
    , treasuryCut
        :: Maybe ApiRational
    , txFeeFixed
        :: Maybe Natural
    , txFeePerByte
        :: Maybe Natural
    , utxoCostPerByte
        :: Maybe ApiLovelace
    , utxoCostPerWord
        :: Maybe ApiLovelace
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON, ToJSON, ToSchema)
        via DefaultSchema ApiProtocolParameters

mkApiProtocolParameters
    :: CardanoApi.ProtocolParameters
    -> ApiProtocolParameters
mkApiProtocolParameters CardanoApi.ProtocolParameters {..} =
    ApiProtocolParameters
        { protocolVersion
            = protocolParamProtocolVersion
            & mkApiProtocolVersion
        , maxTxSize
            = protocolParamMaxTxSize
            & Just
        , maxValueSize
            = protocolParamMaxValueSize
        , txFeePerByte
            = protocolParamTxFeePerByte
            & Just
        , txFeeFixed
            = protocolParamTxFeeFixed
            & Just
        , collateralPercentage
            = protocolParamCollateralPercent
        , maxCollateralInputs
            = protocolParamMaxCollateralInputs
        , stakePoolTargetNum
            = protocolParamStakePoolTargetNum
            & Just
        , costModels
            = protocolParamCostModels
            & Map.mapKeys mkApiPlutusScriptVersion
            & Map.map mkApiCostModel
            & Just
        , prices
            = protocolParamPrices
            & fmap mkApiExecutionUnitPrices
        , maxBlockHeaderSize
            = protocolParamMaxBlockHeaderSize
            & Just
        , maxBlockBodySize
            = protocolParamMaxBlockBodySize
            & Just
        , maxTxExUnits
            = protocolParamMaxTxExUnits
            & fmap mkApiExecutionUnits
        , maxBlockExUnits
            = protocolParamMaxBlockExUnits
            & fmap mkApiExecutionUnits
        , extraPraosEntropy
            = protocolParamExtraPraosEntropy
            & fmap mkApiPraosNonce
        , decentralization
            = protocolParamDecentralization
            & fmap mkApiRational
        , minPoolCost
            = protocolParamMinPoolCost
            & mkApiLovelace
            & Just
        , poolRetireMaxEpoch
            = protocolParamPoolRetireMaxEpoch
            & mkApiEpochNo
            & Just
        , poolPledgeInfluence
            = protocolParamPoolPledgeInfluence
            & mkApiRational
            & Just
        , monetaryExpansion
            = protocolParamMonetaryExpansion
            & mkApiRational
            & Just
        , stakeAddressDeposit
            = protocolParamStakeAddressDeposit
            & mkApiLovelace
            & Just
        , stakePoolDeposit
            = protocolParamStakePoolDeposit
            & mkApiLovelace
            & Just
        , treasuryCut
            = protocolParamTreasuryCut
            & mkApiRational
            & Just
        , minUTxOValue
            = protocolParamMinUTxOValue
            & fmap mkApiLovelace
        , utxoCostPerByte
            = protocolParamUTxOCostPerByte
            & fmap mkApiLovelace
        , utxoCostPerWord
            = protocolParamUTxOCostPerWord
            & fmap mkApiLovelace
        }

genApiProtocolParameters :: Gen ApiProtocolParameters
genApiProtocolParameters = mkApiProtocolParameters
    <$> CardanoApi.genProtocolParameters

printApiProtocolParameters :: ApiProtocolParameters -> IO ()
printApiProtocolParameters = BL.putStrLn . encodePretty

printApiProtocolParametersSchema :: IO ()
printApiProtocolParametersSchema
    = BL.putStrLn
    $ AesonYaml.encode
    $ toSchema (Proxy :: Proxy ApiProtocolParameters)

-- decide on requirements
-- use deriving via to specify defaults
--    Nothings should be omitted
-- make the JSON serializaion exactly the same as Cardano API?
-- try generating values from the schema and parsing them to JSON
-- test that it's possible to read from json both properties.

--------------------------------------------------------------------------------
-- Generic JSON and Schema machinery
--------------------------------------------------------------------------------

newtype DefaultSchema a = DefaultSchema {unDefaultSchema :: a}

defaultOptions :: Aeson.Options
defaultOptions = Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    }

instance
    ( Generic a
    , Aeson.GFromJSON Aeson.Zero (Rep a)
    ) =>
    FromJSON (DefaultSchema a)
  where
    parseJSON a = DefaultSchema
        <$> Aeson.genericParseJSON defaultOptions a

instance
    ( Generic a
    , Aeson.GFromJSON Aeson.Zero (Rep a)
    , Aeson.GFromJSONKey (Rep a)
    ) =>
    FromJSONKey (DefaultSchema a)
  where
    fromJSONKey = DefaultSchema <$>
        Aeson.genericFromJSONKey Aeson.defaultJSONKeyOptions

instance (Generic a, Aeson.GToJSON' Aeson.Value Aeson.Zero (Rep a)) =>
    ToJSON (DefaultSchema a)
  where
    toJSON (DefaultSchema a) =
        Aeson.genericToJSON defaultOptions a

instance
    ( Generic a
    , Aeson.GToJSONKey (Rep a)
    , Aeson.GToJSON' Aeson.Value Aeson.Zero (Rep a)
    ) =>
    ToJSONKey (DefaultSchema a)
  where
    toJSONKey = contramap unDefaultSchema $
        Aeson.genericToJSONKey Aeson.defaultJSONKeyOptions

instance
    ( Generic a
    , OpenApi.GToSchema (Rep a)
    , Typeable a
    ) =>
    ToSchema (DefaultSchema a)
  where
    declareNamedSchema _ =
        OpenApi.genericDeclareNamedSchema @a OpenApi.defaultSchemaOptions Proxy
