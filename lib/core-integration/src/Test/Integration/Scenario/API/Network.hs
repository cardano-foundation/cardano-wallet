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
    ( SyncProgress (..) )
import Control.Monad
    ( forM_ )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , eventually
    , expectErrorMessage
    , expectFieldEqual
    , expectResponseCode
    , request
    , syncProgress
    , verify
    )
import Test.Integration.Framework.TestData
    ( errMsg405 )

import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t. SpecWith (Context t)
spec = do
    it "NETWORK - Can query network information" $ \ctx -> do
        let endpoint = ("GET", "v2/network/information")
        eventually $ do
            r <- request @ApiNetworkInformation ctx endpoint Default Empty
            verify r [ expectFieldEqual syncProgress Ready ]

    describe "NETWORK - v2/network/information - Methods Not Allowed" $ do
        let matrix = ["POST", "CONNECT", "TRACE", "OPTIONS"]
        forM_ matrix $ \method -> it (show method) $ \ctx -> do
            let endpoint = (method, "v2/network/information")
            r <- request @ApiNetworkInformation ctx endpoint Default Empty
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r
