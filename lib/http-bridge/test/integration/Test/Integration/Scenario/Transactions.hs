{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.Transactions
    ( spec
    ) where

import Prelude

import Cardano.Wallet
    ( unsafeRunExceptT )
import Cardano.Wallet.Api.Types
    ( ApiWallet )
import Cardano.Wallet.Compatibility.HttpBridge
    ( HttpBridge )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( ChangeChain (..)
    , FromMnemonic (..)
    , KeyToAddress (..)
    , Passphrase
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , generateKeyFromSeed
    , publicKey
    )
import Cardano.Wallet.Primitive.Types
    ( Address, Coin (..), TxOut (..) )
import Control.Concurrent
    ( threadDelay )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..) )
import Test.Hspec
    ( SpecWith, it )
import Test.Integration.Faucet
    ( mkRedeemTx )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , balanceAvailable
    , balanceTotal
    , expectFieldEqual
    , getFromResponse
    , json
    , request
    , verify
    , walletId
    )

import qualified Cardano.Wallet.Network.HttpBridge as HttpBridge

spec :: SpecWith Context
spec = do
    -- | NOTE
    -- This is a temporary test just to see whether we _can_ indeed create
    -- funded wallets for the integration test scenarios. This will be removed
    -- and refined into a fixture which will setup a bunch of wallets to work
    -- with in the integration test.
    it "Temporary Transaction Test" $ \ctx -> do
        -- | Create a wallet
        let payload = Json [json| {
                "name": "Faucet Wallet #1",
                "mnemonic_sentence": #{mnemonics},
                "passphrase": "cardano-wallet"
                } |]
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        let wid = getFromResponse walletId r

        -- | Get the first address for that wallet
        let (Right seed) = fromMnemonic @'[15] mnemonics
        let (Right pwd) = fromText "cardano-wallet"
        let addr0 = firstAddress (seed, pwd)

        -- | Retrieve funds from the faucet
        nl <- HttpBridge.newNetworkLayer 8080
        unsafeRunExceptT $ postTx nl (mkRedeemTx [ TxOut addr0 (Coin 14000000) ])

        -- | Lookup the new balance
        threadDelay (15 * 1000 * 1000) -- Ugly, pardon me.
        r' <- request @ApiWallet ctx ("GET", "v2/wallets/" <> wid) Default Empty
        verify r'
            [ expectFieldEqual balanceTotal 14000000
            , expectFieldEqual balanceAvailable 14000000
            ]
  where
    mnemonics :: [Text]
    mnemonics =
        ["network", "empty", "cause", "mean", "expire"
        , "private", "finger", "accident", "session", "problem"
        , "absurd", "banner", "stage", "void", "what"
        ]

    firstAddress :: (Passphrase "seed", Passphrase "encryption") -> Address
    firstAddress (seed, pwd) =
        let
            rootXPrv = generateKeyFromSeed (seed, mempty) pwd
            accXPrv = deriveAccountPrivateKey pwd rootXPrv minBound
            addrXPrv = deriveAddressPrivateKey pwd accXPrv ExternalChain minBound
        in
            keyToAddress @HttpBridge (publicKey addrXPrv)
