{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.Transactions
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiWallet )
import Data.Generics.Internal.VL.Lens
    ( view )
import Test.Hspec
    ( SpecWith, it )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , balanceAvailable
    , balanceTotal
    , expectFieldEqual
    , fixtureWallet
    , request
    , verify
    , walletId
    )

spec :: SpecWith Context
spec = do
    it "Temporary Transaction Test" $ \ctx -> do
        wid <- view walletId <$> fixtureWallet ctx
        r' <- request @ApiWallet ctx ("GET", "v2/wallets/" <> wid) Default Empty
        let oneMillionAda = 1 * 1000 * 1000 * 1000 * 1000
        verify r'
            [ expectFieldEqual balanceTotal oneMillionAda
            , expectFieldEqual balanceAvailable oneMillionAda
            ]
