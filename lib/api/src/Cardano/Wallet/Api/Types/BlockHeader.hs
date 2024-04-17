{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright: © 2018-2022 IOHK, 2023 Cardano Foundation
-- License: Apache-2.0

module Cardano.Wallet.Api.Types.BlockHeader
    ( ApiBlockHeader (..)
    , mkApiBlockHeader
    )
    where

import Prelude

import Cardano.Wallet.Api.Aeson
    ( fromTextJSON
    , toTextJSON
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Data.Aeson
    ( FromJSON (parseJSON)
    , ToJSON (toJSON)
    , (.:)
    , (.=)
    )
import Data.Binary
    ( Word32
    , Word64
    )
import Data.Quantity
    ( Quantity (Quantity)
    )
import GHC.Generics
    ( Generic
    )

import qualified Cardano.Wallet.Read as Read
import qualified Cardano.Wallet.Read.Hash as Hash
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8

data ApiBlockHeader = ApiBlockHeader
  { headerHash :: Hash "BlockHeader"
  , slotNo :: Quantity "slot" Word64
  , blockHeight :: Quantity "block" Word32
  }
  deriving (Eq, Show, Generic)

mkApiBlockHeader :: Read.ChainTip -> ApiBlockHeader
mkApiBlockHeader Read.GenesisTip =
    ApiBlockHeader
        { headerHash = Hash $ B8.pack "[genesis]"
        , slotNo = Quantity 0
        , blockHeight = Quantity 0
        }
mkApiBlockHeader Read.BlockTip{slotNo,headerHash,blockNo} =
    ApiBlockHeader
        { headerHash = Hash $ Hash.hashToBytes headerHash
        , slotNo = Quantity $ fromIntegral $ Read.unSlotNo slotNo
        , blockHeight = Quantity $ fromIntegral $ Read.unBlockNo blockNo
        }

instance ToJSON ApiBlockHeader where
    toJSON ApiBlockHeader{..} =
        Aeson.object
            [ "slot_no" .= slotNo
            , "block_height" .= blockHeight
            , "header_hash" .= toTextJSON headerHash
            ]

instance FromJSON ApiBlockHeader where
    parseJSON = Aeson.withObject "ApiBlockHeader" $ \o -> do
        slotNo <- o .: "slot_no"
        blockHeight <- o .: "block_height"
        headerHash <- o .: "header_hash" >>= fromTextJSON "header_hash"
        pure ApiBlockHeader{..}
