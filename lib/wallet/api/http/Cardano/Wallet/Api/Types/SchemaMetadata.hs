{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StrictData #-}

-- |
-- Copyright: © 2018-2022 IOHK, 2023 Cardano Foundation
-- License: Apache-2.0
--
-- A wrapper around TxMetadata to allow different JSON codecs. (ADP-1596)
-- see https://github.com/IntersectMBO/cardano-node/blob/master/cardano-api/src/Cardano/Api/TxMetadata.hs
module Cardano.Wallet.Api.Types.SchemaMetadata where

import Cardano.Api
    ( TxMetadataJsonSchema (..)
    , metadataFromJson
    , metadataToJson
    )
import Cardano.Api.Error
    ( displayError
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxMetadata
    )
import Control.Applicative
    ( (<|>)
    )
import Control.DeepSeq
    ( NFData
    )
import Data.Aeson
    ( FromJSON (parseJSON)
    , ToJSON (toJSON)
    )
import GHC.Generics
    ( Generic
    )
import Prelude

-- | A tag to select the json codec
data TxMetadataSchema = TxMetadataNoSchema | TxMetadataDetailedSchema
    deriving (Show, Eq, Generic, NFData)

-- | A wrapper to drive the json codec of metadata
data TxMetadataWithSchema = TxMetadataWithSchema
    { -- | How to codec the metadata into json
        txMetadataWithSchema_schema :: TxMetadataSchema
    , -- | The metadata
        txMetadataWithSchema_metadata :: TxMetadata
    }
    deriving (Show, Eq, Generic, NFData)

-- | Parses a Boolean "simple-metadata" API flag.
--
-- prop> toSimpleMetadataFlag . parseSimpleMetadataFlag == id
-- prop> parseSimpleMetadataFlag . toSimpleMetadataFlag == id
--
parseSimpleMetadataFlag :: Bool -> TxMetadataSchema
parseSimpleMetadataFlag flag =
    if flag
    then TxMetadataNoSchema
    else TxMetadataDetailedSchema

-- | Produces a Boolean "simple-metadata" API flag.
--
-- prop> toSimpleMetadataFlag . parseSimpleMetadataFlag == id
-- prop> parseSimpleMetadataFlag . toSimpleMetadataFlag == id
--
toSimpleMetadataFlag :: TxMetadataSchema -> Bool
toSimpleMetadataFlag = \case
    TxMetadataNoSchema -> True
    TxMetadataDetailedSchema -> False

instance ToJSON TxMetadataWithSchema where
    toJSON (TxMetadataWithSchema TxMetadataDetailedSchema x) =
        metadataToJson TxMetadataJsonDetailedSchema x
    toJSON (TxMetadataWithSchema TxMetadataNoSchema x) =
        metadataToJson TxMetadataJsonNoSchema x

detailedMetadata :: TxMetadata -> TxMetadataWithSchema
detailedMetadata = TxMetadataWithSchema TxMetadataDetailedSchema

noSchemaMetadata :: TxMetadata -> TxMetadataWithSchema
noSchemaMetadata = TxMetadataWithSchema TxMetadataNoSchema

instance FromJSON TxMetadataWithSchema where
    parseJSON = liftA2
        (<|>)
        (fmap detailedMetadata
            . either (fail . displayError) pure
            . metadataFromJson TxMetadataJsonDetailedSchema
        )
        (fmap noSchemaMetadata
            . either (fail . displayError) pure
            . metadataFromJson TxMetadataJsonNoSchema
        )
