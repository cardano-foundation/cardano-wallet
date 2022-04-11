{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2018-2022 IOHK
-- License: Apache-2.0
--
-- A wrapper around TxMetadata to allow different JSON codecs. (ADP-1596)
-- see https://github.com/input-output-hk/cardano-node/blob/master/cardano-api/src/Cardano/Api/TxMetadata.hs

module Cardano.Wallet.Api.Types.SchemaMetadata where

import Cardano.Api
  ( Error (displayError)
  , TxMetadataJsonSchema (TxMetadataJsonDetailedSchema, TxMetadataJsonNoSchema)
  , metadataFromJson
  , metadataToJson
  )
import Cardano.Wallet.Primitive.Types.Tx
import Control.Applicative (liftA2, (<|>))
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import GHC.Generics (Generic)
import Prelude
import Servant (ToHttpApiData)
import Web.Internal.HttpApiData (toQueryParam, FromHttpApiData, parseQueryParam)
import Data.Function ((&))

-- | a tag to select the json codec
data TxMetadataSchema = TxMetadataNoSchema | TxMetadataDetailedSchema
  deriving (Show, Eq, Generic, NFData)

-- | a wrapper to drive the json codec of metadata
data TxMetadataWithSchema = TxMetadataWithSchema
  { -- | how to codec the metadata into json
    txMetadataWithSchema_schema :: TxMetadataSchema
  , -- | the metadata
    txMetadataWithSchema_metadata :: TxMetadata
  }
  deriving (Show, Eq, Generic, NFData)

instance ToJSON TxMetadataWithSchema where
  toJSON (TxMetadataWithSchema TxMetadataDetailedSchema x) = metadataToJson TxMetadataJsonDetailedSchema x
  toJSON (TxMetadataWithSchema TxMetadataNoSchema x) = metadataToJson TxMetadataJsonNoSchema x

instance FromJSON TxMetadataWithSchema where
  parseJSON v = v & liftA2
    do (<|>)
    do
      fmap (TxMetadataWithSchema TxMetadataDetailedSchema)
        . either (fail . displayError) pure
        . metadataFromJson TxMetadataJsonDetailedSchema
    do
      fmap (TxMetadataWithSchema TxMetadataNoSchema)
        . either (fail . displayError) pure
        . metadataFromJson TxMetadataJsonNoSchema

-- txMetadataSchemaParam :: TxMetadataSchema -> Maybe ()
-- txMetadataSchemaParam TxMetadataDetailedSchema  = Nothing 
-- txMetadataSchemaParam TxMetadataNoSchema  = Just ()

instance ToHttpApiData TxMetadataSchema where 
  toQueryParam = \case     
    TxMetadataNoSchema -> "implicit-types"
    TxMetadataDetailedSchema -> "explicit-types"

instance FromHttpApiData TxMetadataSchema where 
  parseQueryParam = \case 
    "implicit" -> pure TxMetadataNoSchema
    "explicit" -> pure TxMetadataDetailedSchema
    _ -> Left "cannot read metadata schema parameter"