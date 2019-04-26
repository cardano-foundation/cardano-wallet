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
import Data.Text
    ( Text )
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
    , expectListItemFieldEqual
    , expectListSizeEqual
    , expectResponseCode
    , json
    , passphraseLastUpdate
    , request
    , state
    , verify
    , walletId
    , walletName
    )

import qualified Network.HTTP.Types.Status as HTTP

spec :: SpecWith Context
spec = do
    it "WALLETS_CREATE_01 - Create a wallet" $ \ctx -> do

        let payload = Json [json| {
                "name": "1st Wallet",
                "mnemonic_sentence": #{mnemonics15},
                "mnemonic_second_factor": #{mnemonics12},
                "passphrase": "Secure Passphrase",
                "address_pool_gap": 30
                } |]
        r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        verify r
            [ expectResponseCode @IO HTTP.status202
            , expectFieldEqual walletName "1st Wallet"
            , expectFieldEqual addressPoolGap 30
            , expectFieldEqual balanceAvailable 0
            , expectFieldEqual balanceTotal 0
            , expectFieldEqual state (Restoring (Quantity minBound))
            , expectFieldEqual delegation (NotDelegating)
            , expectFieldEqual walletId "2cf060fe53e4e0593f145f22b858dfc60676d4ab"
            , expectFieldNotEqual passphraseLastUpdate "2019-04-12 07:57:28.439742724 UTC"
            ]

    it "WALLETS_CREATE_01 - Created a wallet can be listed" $ \ctx -> do

        let payload = Json [json| {
                "name": "Wallet to be listed",
                "mnemonic_sentence": #{mnemonics18},
                "mnemonic_second_factor": #{mnemonics9},
                "passphrase": "Secure Passphrase",
                "address_pool_gap": 20
                } |]
        _ <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        r2 <- request @[ApiWallet] ctx ("GET", "v2/wallets") Default Empty
        verify r2
            [ expectResponseCode @IO HTTP.status200
            , expectListSizeEqual 1
            , expectListItemFieldEqual 0 walletName "Wallet to be listed"
            , expectListItemFieldEqual 0 addressPoolGap 20
            , expectListItemFieldEqual 0 balanceAvailable 0
            , expectListItemFieldEqual 0 balanceTotal 0
            , expectListItemFieldEqual 0 state (Restoring (Quantity minBound))
            , expectListItemFieldEqual 0 delegation (NotDelegating)
            , expectListItemFieldEqual 0 walletId "dfe87fcf0560fb57937a6468ea51e860672fad79"
            ]

    it "WALLETS_CREATE_03 - Cannot create wallet that exists" $ \ctx -> do
        let payload = Json [json| {
                "name": "Some Wallet",
                "mnemonic_sentence": #{mnemonics21},
                "passphrase": "Secure Passphrase"
                } |]
        r1 <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        expectResponseCode @IO HTTP.status202 r1

        r2 <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
        expectResponseCode @IO HTTP.status409 r2

 where
    -- mnemonics6 :: [Text]
    -- mnemonics6 = ["tornado", "canvas", "peasant", "spike", "enrich", "dilemma"]

    mnemonics9 :: [Text]
    mnemonics9 = ["subway", "tourist", "abstract", "roast", "border", "curious",
        "exercise", "work", "narrow"]

    mnemonics12 :: [Text]
    mnemonics12 = ["agent", "siren", "roof", "water", "giant", "pepper",
        "obtain", "oxygen", "treat", "vessel", "hip", "garlic"]

    mnemonics15 :: [Text]
    mnemonics15 = ["network", "empty", "cause", "mean", "expire", "private",
        "finger", "accident", "session", "problem", "absurd", "banner", "stage",
        "void", "what"]

    mnemonics18 :: [Text]
    mnemonics18 = ["whisper", "control", "diary", "solid", "cattle", "salmon",
        "whale", "slender", "spread", "ice", "shock", "solve", "panel",
        "caution", "upon", "scatter", "broken", "tonight"]

    mnemonics21 :: [Text]
    mnemonics21 = ["click", "puzzle", "athlete", "morning", "fold", "retreat",
        "across", "timber", "essay", "drill", "finger", "erase", "galaxy",
        "spoon", "swift", "eye", "awesome", "shrimp", "depend", "zebra", "token"]

    -- mnemonics24 :: [Text]
    -- mnemonics24 = ["decade", "distance", "denial", "jelly", "wash", "sword",
    --     "olive", "perfect", "jewel", "renew", "wrestle", "cupboard", "record",
    --     "scale", "pattern", "invite", "other", "fruit", "gloom", "west", "oak",
    --     "deal", "seek", "hand"]
