{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}


module Test.Integration.Scenario.Wallets
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiWallet )
import Cardano.Wallet.Primitive.Types
    ( WalletDelegation (..), WalletState (..) )
import Data.Quantity
    ( Quantity (..) )
import qualified Network.HTTP.Types.Status as HTTP
import Test.Hspec
    ( SpecWith, it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , addressPoolGap
    , balanceAvailable
    , balanceTotal
    , delegation
    , expectFieldEqual
    , expectFieldNotEqual
    , expectResponseCode
    , json
    , passphraseLastUpdate
    , request
    , state
    , walletId
    , walletName
    )

spec :: SpecWith Context
spec = do
    it "Create a wallet" $ \ctx -> do

        let payload = Json [json| {
                "name": "1st Wallet",
                "mnemonic_sentence": ["identify", "screen", "lock", "bargain", "inch", "drop", "canyon", "flock", "dry", "zone", "wash", "argue", "system", "glory", "light"],
                "mnemonic_second_factor": ["attract", "tornado", "slender", "pumpkin", "clown", "announce", "term", "winner", "ready"],
                "passphrase": "Secure Passphrase",
                "address_pool_gap": 20
                } |]
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        expectResponseCode @IO HTTP.status200 r
        expectFieldEqual walletName "1st Wallet" r
        expectFieldEqual addressPoolGap 20 r
        expectFieldEqual balanceAvailable 0 r
        expectFieldEqual balanceTotal 0 r
        expectFieldEqual delegation (NotDelegating) r
        expectFieldEqual state (Restoring (Quantity minBound)) r
        expectFieldEqual walletId "4bba596017a63e52a4cf6caaadf8a670bfe4745b" r
        expectFieldNotEqual passphraseLastUpdate "2019-04-12 07:57:28.439742724 UTC" r
