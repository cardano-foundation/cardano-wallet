{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright: © 2018-2022 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Api.Types.BlockHeader
  ( ApiBlockHeader (..)
  , mkApiBlockHeader
  )
where

import Cardano.Wallet.Api.Aeson
  ( fromTextJSON
  , toTextJSON
  )
import Cardano.Wallet.Primitive.Types
  ( BlockHeader (..)
  , SlotNo (..)
  )
import Cardano.Wallet.Primitive.Types.Hash
  ( Hash
  )
import Data.Aeson
  ( FromJSON (parseJSON)
  , ToJSON (toJSON)
  , (.:)
  , (.=)
  )
import Data.Aeson qualified as Aeson
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
import Prelude

data ApiBlockHeader = ApiBlockHeader
  { headerHash :: Hash "BlockHeader"
  , slotNo :: Quantity "slot" Word64
  , blockHeight :: Quantity "block" Word32
  }
  deriving (Eq, Show, Generic)

mkApiBlockHeader :: BlockHeader -> ApiBlockHeader
mkApiBlockHeader BlockHeader {..} =
  ApiBlockHeader
    { slotNo = Quantity $ unSlotNo slotNo
    , ..
    }

instance ToJSON ApiBlockHeader where
  toJSON ApiBlockHeader {..} =
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
    pure ApiBlockHeader {..}
