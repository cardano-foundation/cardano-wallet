{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.HWWallets
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiTransaction, ApiWallet, WalletStyle (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( FromMnemonic (..)
    , HardDerivation (..)
    , NetworkDiscriminant (..)
    , PersistPublicKey (..)
    , WalletKey (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( generateKeyFromSeed )
import Cardano.Wallet.Primitive.Mnemonic
    ( entropyToMnemonic, genEntropy, mnemonicToText )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Quantity
    ( Quantity (..) )
import Test.Hspec
    ( SpecWith, it, shouldBe )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , eventually
    , expectField
    , expectResponseCode
    , fixtureWallet
    , getFromResponse
    , json
    , listAddresses
    , request
    , verify
    )
import Test.Integration.Framework.TestData
    ( payloadWith )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types.Status as HTTP


spec :: forall t n. (n ~ 'Testnet) => SpecWith (Context t)
spec = do
    it "HW_WALLETS_01 - Restoration from account public key preserve funds" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        -- create wallet
        mnemonics <- mnemonicToText @15 . entropyToMnemonic <$> genEntropy
        let wName = "!st created"
        let payldCrt = payloadWith wName mnemonics
        rInit <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default payldCrt
        verify rInit
            [ expectResponseCode @IO HTTP.status201
            , expectField (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
            , expectField (#balance . #getApiT . #total) (`shouldBe` Quantity 0)
            ]

        --send funds
        let wDest = getFromResponse id rInit
        addrs <- listAddresses ctx wDest
        let destination = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": 1,
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]
        rTrans <- request @(ApiTransaction n) ctx (Link.createTransaction wSrc)
            Default payload
        expectResponseCode @IO HTTP.status202 rTrans

        eventually "Wallet balance is as expected" $ do
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rGet
                [ expectField
                        (#balance . #getApiT . #total) (`shouldBe` Quantity 1)
                , expectField
                        (#balance . #getApiT . #available) (`shouldBe` Quantity 1)
                ]

        -- delete wallet
        rDel <-
            request @ApiWallet ctx (Link.deleteWallet @'Shelley wDest) Default Empty
        expectResponseCode @IO HTTP.status204 rDel

        -- restore from account public key and make sure funds are there
        let (Right seed) = fromMnemonic @'[15] mnemonics
        let rootXPrv = generateKeyFromSeed (seed, Nothing) mempty
        let accXPub = T.decodeUtf8 $ serializeXPub $
                publicKey $ deriveAccountPrivateKey mempty rootXPrv minBound
        print mnemonics
        print accXPub
        let payloadRestore = Json [json| {
                "name": #{wName},
                "account_public_key": #{accXPub}
            }|]
        rRestore <-
            request @ApiWallet ctx (Link.postWallet @'Shelley) Default payloadRestore
        let wDest' = getFromResponse id rRestore
        expectResponseCode @IO HTTP.status201 rRestore

        eventually "Balance of restored wallet is as expected" $ do
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest') Default Empty
            verify rGet
                [ expectField
                        (#balance . #getApiT . #total) (`shouldBe` Quantity 1)
                , expectField
                        (#balance . #getApiT . #available) (`shouldBe` Quantity 1)
                ]
