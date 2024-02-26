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
import Data.Text.Class
    ( FromText (..)
    , ToText (..)
    )

type ApiRestorationMode = ApiT RestorationMode

instance ToJSON ApiRestorationMode where
    toJSON :: ApiRestorationMode -> Value
    toJSON =
        getApiT >>> \case
            RestoreFromGenesis -> String "from_genesis"
            RestoreFromTip -> String "from_tip"
            RestoreFromBlock b s ->
                object
                    [ "block_header_hash" .= String (toText b)
                    , "absolute_slot_number" .= s
                    ]

instance FromJSON ApiRestorationMode where
    parseJSON :: Value -> Parser ApiRestorationMode
    parseJSON =
        fmap ApiT . \case
            String "from_genesis" -> pure RestoreFromGenesis
            String "from_tip" -> pure RestoreFromTip
            Object obj -> do
                block <- obj .: "block_header_hash"
                slot <- obj .: "absolute_slot_number"
                case fromText block of
                    Right b -> pure $ RestoreFromBlock b slot
                    Left r -> fail $ "Invalid block hash: " <> show r
            _ -> fail "Invalid restoration mode"
