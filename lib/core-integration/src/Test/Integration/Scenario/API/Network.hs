{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.API.Network
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiNetworkInformation )
import Cardano.Wallet.Primitive.Types
    ( NtpStatus (..), ProtocolUpdates (..), SyncProgress (..) )
import Test.Hspec
    ( SpecWith, it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , eventually
    , expectFieldEqual
    , ntpStatus
    , protocolUpdates
    , request
    , syncProgress
    , verify
    )

spec :: forall t. SpecWith (Context t)
spec = do
    it "NETWORK - Can query network information" $ \ctx -> do
        let endpoint = ("GET", "v2/network/information")
        eventually $ do
            r <- request @ApiNetworkInformation ctx endpoint Default Empty
            verify r
                [ expectFieldEqual syncProgress Ready
                , expectFieldEqual protocolUpdates UpdatesUnavailable
                , expectFieldEqual ntpStatus NtpUnavailable
                ]
