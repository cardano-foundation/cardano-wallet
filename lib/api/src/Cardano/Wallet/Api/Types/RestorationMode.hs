{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Api.Types.RestorationMode
    ( ApiRestorationMode
    ) where

import Prelude

import Cardano.Wallet.Api.Lib.ApiT
    ( ApiT (..)
    )
import Cardano.Wallet.Network.RestorationMode
    ( RestorationMode (..)
    )
import Control.Category
    ( (>>>)
    )
import Data.Aeson
    ( FromJSON (parseJSON)
    , KeyValue (..)
    , ToJSON (toJSON)
    , Value (..)
    , object
    , (.:)
    )
import Data.Aeson.Types
    ( Parser
    )

import qualified Cardano.Wallet.Read as Read
import qualified Cardano.Wallet.Read.Hash as Hash

type ApiRestorationMode = ApiT RestorationMode

instance ToJSON ApiRestorationMode where
    toJSON :: ApiRestorationMode -> Value
    toJSON =
        getApiT >>> \case
            RestoreFromGenesis -> String "from_genesis"
            RestoreFromTip -> String "from_tip"
            RestoreFromBlock (Read.SlotNo s) b ->
                object
                    [ "block_header_hash" .= String (Hash.hashToTextAsHex b)
                    , "absolute_slot_number" .= s
                    ]

instance FromJSON ApiRestorationMode where
    parseJSON :: Value -> Parser ApiRestorationMode
    parseJSON =
        fmap ApiT . \case
            String "from_genesis" -> pure RestoreFromGenesis
            String "from_tip" -> pure RestoreFromTip
            Object obj -> do
                blockhash <- obj .: "block_header_hash"
                slot <- obj .: "absolute_slot_number"
                case Hash.hashFromTextAsHex blockhash of
                    Just b -> pure $ RestoreFromBlock (Read.SlotNo slot) b
                    Nothing -> fail $ "Invalid block hash: " <> show blockhash
            _ -> fail "Invalid restoration mode"
