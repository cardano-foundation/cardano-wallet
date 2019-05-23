{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.Addresses
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiAddress )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge )
import Cardano.Wallet.Primitive.Types
    ( AddressState (..) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Test.Hspec
    ( SpecWith, it )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , expectListItemFieldEqual
    , expectListSizeEqual
    , fixtureWallet
    , request
    , state
    , verify
    , walletId
    )

spec :: SpecWith Context
spec = do
    it "Can list known addresses on a default wallet" $ \ctx -> do
        wid <- view walletId <$> fixtureWallet ctx
        let endpoint = ("GET", "v2/wallets/" <> wid <> "/addresses")
        r <- request @[ApiAddress HttpBridge] ctx endpoint Default Empty
        verify r
            [ expectListSizeEqual 21
            , expectListItemFieldEqual 0 state Used
            , expectListItemFieldEqual 1 state Unused
            , expectListItemFieldEqual 2 state Unused
            , expectListItemFieldEqual 3 state Unused
            -- And so forth...
            , expectListItemFieldEqual 5 state Unused
            , expectListItemFieldEqual 8 state Unused
            , expectListItemFieldEqual 13 state Unused
            , expectListItemFieldEqual 20 state Unused
            ]
