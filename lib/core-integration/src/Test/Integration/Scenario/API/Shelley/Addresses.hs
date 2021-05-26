{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.Shelley.Addresses
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( AnyAddress
    , ApiAccountKey
    , ApiAddress
    , ApiT (..)
    , ApiTransaction
    , ApiVerificationKeyShelley
    , ApiWallet
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationIndex (..), Role (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( defaultAddressPoolGap, getAddressPoolGap, purposeCIP1852 )
import Cardano.Wallet.Primitive.Types.Address
    ( AddressState (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxStatus (..) )
import Control.Monad
    ( forM, forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Aeson
    ( ToJSON (..), object, (.=) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Test.Hspec
    ( SpecWith, describe )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldNotSatisfy, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , emptyRandomWallet
    , emptyWallet
    , emptyWalletWith
    , eventually
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectResponseCode
    , fixturePassphrase
    , fixtureWallet
    , getFromResponse
    , isValidDerivationPath
    , json
    , listAddresses
    , minUTxOValue
    , request
    , unsafeRequest
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg400ScriptDuplicateKeys
    , errMsg400ScriptIllFormed
    , errMsg400ScriptNotUniformRoles
    , errMsg400ScriptTimelocksContradictory
    , errMsg400ScriptWrongCoeffcient
    , errMsg403WrongIndex
    , errMsg404NoWallet
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as Aeson
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall n.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    ) => SpecWith Context
spec = describe "SHELLEY_ADDRESSES" $ do
    it "BYRON_ADDRESS_LIST - Byron wallet on Shelley ep" $ \ctx -> runResourceT $ do
        w <- emptyRandomWallet ctx
        let wid = w ^. walletId
        let ep = ("GET", "v2/wallets/" <> wid <> "/addresses")
        r <- request @[ApiAddress n] ctx ep Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet wid) r

    it "ADDRESS_LIST_01 - Can list known addresses on a default wallet" $ \ctx -> runResourceT $ do
        let g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
        w <- emptyWallet ctx
        r <- request @[ApiAddress n] ctx
            (Link.listAddresses @'Shelley w) Default Empty
        expectResponseCode HTTP.status200 r
        expectListSize g r
        forM_ [0..(g-1)] $ \addrNum -> do
            expectListField addrNum (#state . #getApiT) (`shouldBe` Unused) r
            expectListField addrNum #derivationPath
                (`shouldSatisfy` (isValidDerivationPath purposeCIP1852)) r

    it "ADDRESS_LIST_01 - Can list addresses with non-default pool gap" $ \ctx -> runResourceT $ do
        let g = 15
        w <- emptyWalletWith ctx ("Wallet", fixturePassphrase, g)
        r <- request @[ApiAddress n] ctx
            (Link.listAddresses @'Shelley w) Default Empty
        expectResponseCode HTTP.status200 r
        expectListSize g r
        forM_ [0..(g-1)] $ \addrNum -> do
            expectListField addrNum (#state . #getApiT) (`shouldBe` Unused) r
            expectListField addrNum #derivationPath
                (`shouldSatisfy` (isValidDerivationPath purposeCIP1852)) r

    it "ADDRESS_LIST_02 - Can filter used and unused addresses" $ \ctx -> runResourceT $ do
        let g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
        w <- fixtureWallet ctx
        rUsed <- request @[ApiAddress n] ctx
            (Link.listAddresses' @'Shelley w (Just Used)) Default Empty
        expectResponseCode HTTP.status200 rUsed
        expectListSize 10 rUsed
        forM_ [0..9] $ \addrNum -> do
            expectListField
                addrNum (#state . #getApiT) (`shouldBe` Used) rUsed
        rUnused <- request @[ApiAddress n] ctx
            (Link.listAddresses' @'Shelley w (Just Unused)) Default Empty
        expectResponseCode HTTP.status200 rUnused
        expectListSize g rUnused
        forM_ [10..(g-1)] $ \addrNum -> do
            expectListField
                addrNum (#state . #getApiT) (`shouldBe` Unused) rUnused

    it "ADDRESS_LIST_02 - Shows nothing when there are no used addresses"
        $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        rUsed <- request @[ApiAddress n] ctx
            (Link.listAddresses' @'Shelley w (Just Used)) Default Empty
        rUnused <- request @[ApiAddress n] ctx
            (Link.listAddresses' @'Shelley w (Just Unused)) Default Empty
        expectResponseCode HTTP.status200 rUsed
        expectListSize 0 rUsed
        expectResponseCode HTTP.status200 rUnused
        expectListSize 20 rUnused
        forM_ [0..19] $ \addrNum -> do
            expectListField
                addrNum (#state . #getApiT) (`shouldBe` Unused) rUnused

    -- TODO
    -- MOVE TO test/unit/Cardano/Wallet/ApiSpec.hs
    describe "ADDRESS_LIST_02 - Invalid filters are bad requests" $ do
        let filters =
                [ "usedd"
                , "uused"
                , "unusedd"
                , "uunused"
                , "USED"
                , "UNUSED"
                , "-1000"
                , "44444444"
                , "*"
                ]

        let withQuery f (method, link) = (method, link <> "?state=" <> T.pack f)
        forM_ filters $ \fil -> it fil $ \ctx -> runResourceT $ do
            w <- emptyWallet ctx
            let link = withQuery fil $ Link.listAddresses @'Shelley w
            r <- request @[ApiAddress n] ctx link Default Empty
            verify r
                [ expectResponseCode HTTP.status400
                , expectErrorMessage
                    "Error parsing query parameter state failed: Unable to\
                    \ decode the given text value. Please specify\
                    \ one of the following values: used, unused."
                ]

    it "ADDRESS_LIST_03 - Generates new address pool gap" $ \ctx -> runResourceT $ do
        let initPoolGap = 10
        wSrc <- fixtureWallet ctx
        wDest <- emptyWalletWith ctx ("Wallet", fixturePassphrase, initPoolGap)

        -- make sure all addresses in address_pool_gap are 'Unused'
        r <- request @[ApiAddress n] ctx
            (Link.listAddresses @'Shelley wDest) Default Empty
        verify r
            [ expectResponseCode HTTP.status200
            , expectListSize initPoolGap
            ]
        forM_ [0..9] $ \addrNum -> do
            expectListField addrNum (#state . #getApiT) (`shouldBe` Unused) r
        addrs <- listAddresses @n ctx wDest

        -- run 10 transactions to make all addresses `Used`
        forM_ [0..9] $ \addrNum -> do
            let destination = (addrs !! addrNum) ^. #id
            let payload = Json [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": #{minUTxOValue},
                            "unit": "lovelace"
                        }
                    }],
                    "passphrase": #{fixturePassphrase}
                }|]

            rTrans <- request @(ApiTransaction n) ctx
                (Link.createTransaction @'Shelley wSrc) Default payload
            expectResponseCode HTTP.status202 rTrans

        -- make sure all transactions are in ledger
        eventually "Wallet balance = initPoolGap * minUTxOValue" $ do
            rb <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            expectField
                (#balance . #available)
                (`shouldBe` Quantity (10 * 1_000_000))
                rb

        -- verify new address_pool_gap has been created
        rAddr <- request @[ApiAddress n] ctx
            (Link.listAddresses @'Shelley wDest) Default Empty
        verify rAddr
            [ expectResponseCode HTTP.status200
            , expectListSize 20
            ]
        forM_ [0..9] $ \addrNum -> do
            expectListField
                addrNum (#state . #getApiT) (`shouldBe` Used) rAddr
        forM_ [10..19] $ \addrNum -> do
            expectListField
                addrNum (#state . #getApiT) (`shouldBe` Unused) rAddr

    it "ADDRESS_LIST_04 - Deleted wallet" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx
            (Link.deleteWallet @'Shelley w) Default Empty
        r <- request @[ApiAddress n] ctx
            (Link.listAddresses @'Shelley w) Default Empty
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "ADDRESS_LIST_05 - bech32 HRP is correct - mainnet" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        r <- request @[Aeson.Value] ctx
            (Link.listAddresses @'Shelley w) Default Empty
        verify r
            [ expectResponseCode HTTP.status200
            -- integration tests are configured for mainnet
            , expectListField 0 (Aeson.key "id" . Aeson._String)
                (`shouldSatisfy` T.isPrefixOf "addr")
            , expectListField 0 (Aeson.key "id" . Aeson._String)
                (`shouldNotSatisfy` T.isPrefixOf "addr_test")
            ]

    it "ADDRESS_LIST_06 - Used change addresses are listed after a transaction is no longer pending" $ \ctx -> runResourceT @IO $ do
        let verifyAddrs nTotal nUsed addrs = do
                liftIO (length addrs `shouldBe` nTotal)
                let onlyUsed = filter ((== Used) . (^. (#state . #getApiT))) addrs
                liftIO (length onlyUsed `shouldBe` nUsed)

        -- 1. Create Shelley wallets
        let initialTotalA = 30
        let initialUsedA  = 10
        wA <- fixtureWallet ctx
        listAddresses @n ctx wA
            >>= verifyAddrs initialTotalA initialUsedA

        let initialTotalB = 20
        let initialUsedB  = 0
        wB <- emptyWallet ctx
        listAddresses @n ctx wB
            >>= verifyAddrs initialTotalB initialUsedB

        -- 2. Send a transaction from A -> B
        destination <- view #id . head <$> listAddresses @n ctx wB
        let amount = 10 * minUTxOValue
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amount},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{fixturePassphrase}
            }|]
        (_, rtx) <- unsafeRequest @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wA) payload

        -- 3. Check that there's one more used addresses on A.
        --
        -- Ideally, we would also like to check that there's no used address on
        -- B yet, but this would make the test quite flaky. Indeed, the integration
        -- tests produces block very fast and by the time we make this call the
        -- transaction may have already been inserted in the ledger and
        -- discovered by B.
        --
        -- Similarly, we can't assert the length of used addresses on A. It
        -- _should_ be 'initialUsedA` but the transaction could have already
        -- been inserted and discovered by the time the 'listAddresses' call
        -- resolves.
        listAddresses @n ctx wA
            >>= \addrs -> liftIO $ length addrs `shouldBe` (initialTotalA + 1)

        -- 4. Wait for transaction from A -> B to no longer be pending
        eventually "Transaction from A -> B is discovered on B" $ do
            request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shelley wA rtx) Default Empty
                >>= expectField #status (`shouldBe` ApiT InLedger)
            request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shelley wB rtx) Default Empty
                >>= expectField #status (`shouldBe` ApiT InLedger)

        -- 5. Check that there's one more used and total addresses on the wallets
        -- A and B.
        --
        -- On A: The address comes from the internal pool gap and was hidden up
        --       until the transaction is created and remains after it is
        --       inserted.
        --
        -- On B: There's a new total address because the address used was the
        --       first unused address from the consecutive sequence of the address
        --       pool. Thus the address window was shifted be exactly one.
        listAddresses @n ctx wA
            >>= verifyAddrs (initialTotalA + 1) (initialUsedA + 1)
        listAddresses @n ctx wB
            >>= verifyAddrs (initialTotalB + 1) (initialUsedB + 1)

    it "ADDRESS_INSPECT_01 - Address inspect OK" $ \ctx -> do
        let str = "Ae2tdPwUPEYz6ExfbWubiXPB6daUuhJxikMEb4eXRp5oKZBKZwrbJ2k7EZe"
        r <- request @Aeson.Value ctx (Link.inspectAddress str) Default Empty
        expectResponseCode HTTP.status200 r

    it "ADDRESS_INSPECT_02 - Address inspect KO" $ \ctx -> runResourceT $ do
        let str = "patate"
        r <- request @Aeson.Value ctx (Link.inspectAddress str) Default Empty
        expectResponseCode HTTP.status400 r

    it "ADDRESS_INSPECT_03 - Address inspect bech32" $ \ctx -> do
        let str = "addr_test1qzamu40sglnsrylzv9jylekjmzgaqsg5v5z9u6yk3jpnnxjwck77fqu8deuumsvnazjnjhwasc2eetfqpa2pvygts78ssd5388"
        r <- request @Aeson.Value ctx (Link.inspectAddress str) Default Empty
        verify r
            [ expectResponseCode HTTP.status200
            , expectField (Aeson.key "spending_key_hash_bech32" . Aeson._String)
                (`shouldBe` "addr_vkh1hwl9tuz8uuqe8cnpv387d5kcj8gyz9r9q30x395vsvue5e44fh7")
            , expectField (Aeson.key "stake_key_hash_bech32" . Aeson._String)
                (`shouldBe` "stake_vkh1fmzmmeyrsah8nnwpj0522w2amkrpt89dyq84g9s3pwrc7dqjnfu")
            ]

    -- Generating golden test data for enterprise addresses - script credential:
    --- $ cardano-address script hash "$(cat script.txt)" \
    --- | cardano-address address payment --from-script --network-tag mainnet
    it "ANY_ADDRESS_POST_01 - Golden tests for enterprise script address - signature" $ \ctx -> do
        --- $ cat script.txt
        --- addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq
        let payload = Json [json|{
                "payment": "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq"
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r
        let goldenAddr =
                "addr1w8rqr8fmk4ulc7pgycd96fuqcg40e5ecuway0ypc2tsnteqm5wul2" :: Text
        validateAddr r goldenAddr

    it "ANY_ADDRESS_POST_02 - Golden tests for enterprise script address - any" $ \ctx -> do
        --- $ cat script.txt
        --- any [addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq, addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp]
        let payload = Json [json|{
                "payment": {
                    "any": [
                        "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                        "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp"
                        ]
                    }
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r
        let goldenAddr =
                "addr1w8jtlgneqelxxlckcsgrkd7rd6ycrgegu5th24x0f058gmqhsnv92" :: Text
        validateAddr r goldenAddr

    it "ANY_ADDRESS_POST_03 - Golden tests for enterprise script address - all" $ \ctx -> do
        --- $ cat script.txt
        --- all [addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq, addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp]
        let payload = Json [json|{
                "payment": {
                    "all": [
                        "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                        "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp"
                        ]
                    }
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r
        let goldenAddr =
                "addr1w9q0ghwy73wapjcdwqxm6ytwe66j8eccsmn9jptshrjerasvf2cg0" :: Text
        validateAddr r goldenAddr

    it "ANY_ADDRESS_POST_04 - Golden tests for enterprise script address - some" $ \ctx -> do
        --- $ cat script.txt
        --- at_least 2 [addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq,addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp,addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9]
        let payload = Json [json|{
                "payment": {
                    "some": {
                        "from" : [
                            "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                            "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp",
                            "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9"
                            ],
                         "at_least": 2
                         }
                    }
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r
        let goldenAddr =
                "addr1wyqmnmwuh85e0fxaggl6ac2hfeqncg76gsr0ld8qdjd84ag6sm0n8" :: Text
        validateAddr r goldenAddr

    -- Generating golden test data for reward account addresses - script credential:
    --- $ cardano-address script hash "$(cat script.txt)" \
    --- | cardano-address address stake --from-script --network-tag mainnet
    it "ANY_ADDRESS_POST_05 - Golden tests for reward account script address - any" $ \ctx -> do
        let payload = Json [json|{
                "stake": {
                    "any": [
                        "stake_shared_vkh1nqc00hvlc6cq0sfhretk0rmzw8dywmusp8retuqnnxzajtzhjg5",
                        "stake_shared_vkh1nac0awgfa4zjsh4elnjmsscz0huhss8q2g0x3n7m539mwaa5m7s"
                        ]
                    }
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r
        let goldenAddr =
                "stake17yshpfjkgh98wumvnn9y3yfhevllp4y04u6y84q3flxcv9sduxphm" :: Text
        validateAddr r goldenAddr

    -- Generating golden test data for reward account addresses - both script credentials:
    --- $ cardano-address script hash "$(cat script1.txt)" \
    --- | cardano-address address payment --from-script --network-tag mainnet \
    --- | cardano-address address delegation --from-script $(cardano-address script hash "$(cat script2.txt)")
    it "ANY_ADDRESS_POST_06 - Golden tests for delegating script address - any" $ \ctx -> do
        let payload = Json [json|{
                "payment": {
                    "some": {
                        "from" : [
                            "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                            "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp",
                            "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9"
                            ],
                         "at_least": 2
                         }
                    },
                "stake": {
                    "any": [
                        "stake_shared_vkh1nqc00hvlc6cq0sfhretk0rmzw8dywmusp8retuqnnxzajtzhjg5",
                        "stake_shared_vkh1nac0awgfa4zjsh4elnjmsscz0huhss8q2g0x3n7m539mwaa5m7s"
                        ]
                    }
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r
        let goldenAddr =
                "addr1xyqmnmwuh85e0fxaggl6ac2hfeqncg76gsr0ld8qdjd84afpwzn9v3w2waeke8x2fzgn0jel7r2glte5g02pzn7dsctqu6mtx3" :: Text
        validateAddr r goldenAddr

    -- Generating golden test data for enterprise addresses - key credential:
    --- $ cat recovery-phrase.txt
    --- east student silly already breeze enact seat trade few way online skin grass humble electric
    --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
    --- > | cardano-address key child 1852H/1815H/0H/0/0 \
    --- > | cardano-address key public --without-chain-code
    --- xpub1lqglg77z6kajsdz4739q22c0zm0yhuy567z6xk2vc0z5ucjtkwps75l8wa
    -- which can be translated in cardano-addresses
    -- :set -XOverloadedStrings
    -- import Data.Text
    -- let k = "xpub1lqglg77z6kajsdz4739q22c0zm0yhuy567z6xk2vc0z5ucjtkwps75l8wa" :: Text
    -- let (Right bytes) = fromBech32 (const id) (T.encodeUtf8 k)
    -- bytes
    -- "\248\DC1\244{\194\213\187(4U\244J\ENQ+\SI\SYN\222K\240\148\215\133\163YL\195\197NbK\179\131"
    -- let (Right hrp) = Bech32.humanReadablePartFromText "addr_vk"
    -- encode (EBech32 hrp) bytes
    -- "addr_vk1lqglg77z6kajsdz4739q22c0zm0yhuy567z6xk2vc0z5ucjtkwpschzd2j"

    -- Golden address can be obtained via
    --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
    --- > | cardano-address key child 1852H/1815H/0H/0/0 \
    --- > | cardano-address key public --with-chain-code \
    --- > | cardano-address address payment --from-key --network-tag mainnet
    it "ANY_ADDRESS_POST_07 - Golden tests for enterprise pub key address" $ \ctx -> do
        let payload = Json [json|{
                "payment": "addr_vk1lqglg77z6kajsdz4739q22c0zm0yhuy567z6xk2vc0z5ucjtkwpschzd2j"
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r
        let goldenAddr =
                "addr1v9qthemrg5kczwfjjnahwt65elhrl95e9hcgufnajtp6wfgknj82e" :: Text
        validateAddr r goldenAddr

    -- Generating golden test data for enterprise addresses - key credential:
    --- $ cat recovery-phrase.txt
    --- east student silly already breeze enact seat trade few way online skin grass humble electric
    --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
    --- > | cardano-address key child 1852H/1815H/0H/2/0 \
    --- > | cardano-address key public --without-chain-code
    --- xpub16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7qh83kg9
    -- which can be translated in cardano-addresses
    -- :set -XOverloadedStrings
    -- import Data.Text
    -- let k = "xpub16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7qh83kg9" :: Text
    -- let (Right bytes) = fromBech32 (const id) (T.encodeUtf8 k)
    -- bytes
    -- "\215C\220\206e\226\245\n\191\248t~vh\230\235\190?\129O\DC2\139\173pX1\229\182(\ENQ\225<"
    -- let (Right hrp) = Bech32.humanReadablePartFromText "stake_vk"
    -- encode (EBech32 hrp) bytes
    -- "stake_vk16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7qau558d"

    -- Golden address can be obtained via
    --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
    --- > | cardano-address key child 1852H/1815H/0H/2/0 \
    --- > | cardano-address key public --with-chain-code \
    --- > | cardano-address address stake --from-key --network-tag mainnet
    it "ANY_ADDRESS_POST_08 - Golden tests for reward account pub key address" $ \ctx -> do
        let payload = Json [json|{
                "stake": "stake_vk16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7qau558d"
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r
        let goldenAddr =
                "stake1uy6pmlvyl3wn4ca6807e26gy2gek9hqu0gastzh5tk0xx0g2rxsr5" :: Text
        validateAddr r goldenAddr

    -- Golden address can be obtained via
    --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
    --- > | cardano-address key child 1852H/1815H/0H/2/0 \
    --- > | cardano-address key public --with-chain-code > stake.xpub

    --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
    --- > | cardano-address key child 1852H/1815H/0H/0/0 \
    --- > | cardano-address key public --with-chain-code \
    --- > | cardano-address address payment --from-key --network-tag mainnet \
    --- > | cardano-address address delegation --from-key $(cat stake.xpub)
    it "ANY_ADDRESS_POST_09 - Golden tests for delegating address with both pub key credentials" $ \ctx -> do
        let payload = Json [json|{
                "payment": "addr_vk1lqglg77z6kajsdz4739q22c0zm0yhuy567z6xk2vc0z5ucjtkwpschzd2j",
                "stake": "stake_vk16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7qau558d"
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r
        let goldenAddr =
                "addr1q9qthemrg5kczwfjjnahwt65elhrl95e9hcgufnajtp6wff5r\
                \h7cflza8t3m5wlaj45sg53nvtwpc73mqk90ghv7vv7s64ryn2" :: Text
        validateAddr r goldenAddr

    -- Generating golden test data for delegating address - payment from script, stake from pub key:
    --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
    --- > | cardano-address key child 1852H/1815H/0H/2/0 \
    --- > | cardano-address key public --with-chain-code > stake.xpub

    --- $ cardano-address script hash "$(cat script.txt)" \
    --- | cardano-address address payment --from-script --network-tag mainnet \
    --- | cardano-address address delegation --from-key $(cat stake.xpub)
    it "ANY_ADDRESS_POST_10 - Golden tests for delegating address - payment from script, stake from key" $ \ctx -> do
        let payload = Json [json|{
                "payment": {
                    "some": {
                        "from" : [
                            "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                            "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp",
                            "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9"
                            ],
                         "at_least": 2
                         }
                     },
                "stake": "stake_vk16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7qau558d"
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r
        let goldenAddr =
                "addr1zyqmnmwuh85e0fxaggl6ac2hfeqncg76gsr0ld8qdjd84af5rh7cflza8t3m5wlaj45sg53nvtwpc73mqk90ghv7vv7srr0dle" :: Text
        validateAddr r goldenAddr

    -- Generating golden test data for delegating address - payment from pub key, stake from script:
    --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
    --- > | cardano-address key child 1852H/1815H/0H/0/0 \
    --- > | cardano-address key public --with-chain-code \
    --- > | cardano-address address payment --from-key --network-tag mainnet \
    --- > | cardano-address address delegation --from-script $(cardano-address script hash "$(cat script3.txt)")
    it "ANY_ADDRESS_POST_11 - Golden tests for delegating address - payment from key, stake from script" $ \ctx -> do
        let payload = Json [json|{
                "payment": "addr_vk1lqglg77z6kajsdz4739q22c0zm0yhuy567z6xk2vc0z5ucjtkwpschzd2j",
                "stake": {
                    "some": {
                        "from" : [
                            "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                            "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp",
                            "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9"
                            ],
                         "at_least": 2
                         }
                    }
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r
        let goldenAddr =
                "addr1y9qthemrg5kczwfjjnahwt65elhrl95e9hcgufnajtp6wfgph8kaew0fj7jd6s3l4ms4wnjp8s3a53qxl76wqmy60t6ssqcamq" :: Text
        validateAddr r goldenAddr

    it "ANY_ADDRESS_POST_12 - Delegating addresses API roundtrip" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx

        -- Generate first 20 addresses using payment and stake keys derived from
        -- wallet API
        let indices = [0..19]
        generatedAddresses <- forM indices $ \index -> do
            let paymentPath = Link.getWalletKey @'Shelley w UtxoExternal (DerivationIndex index) Nothing
            (_, paymentKey) <- unsafeRequest @ApiVerificationKeyShelley ctx paymentPath Empty

            let stakePath = Link.getWalletKey @'Shelley w MutableAccount (DerivationIndex 0) Nothing
            (_, stakeKey) <- unsafeRequest @ApiVerificationKeyShelley ctx stakePath Empty

            let payload = Json [json|{
                    "payment": #{paymentKey},
                    "stake": #{stakeKey}
                }|]
            (_, addr) <- unsafeRequest @AnyAddress ctx Link.postAnyAddress payload
            pure (addr ^. #payload)

        -- Make sure the same addresses are already available in the wallet
        addrs <- listAddresses @n ctx w
        forM_ (zip (fmap fromIntegral indices) generatedAddresses)
            $ \(idx, genAddr) -> do
                let walAddr = fst (addrs !! idx ^. #id) ^. (#getApiT . #unAddress)
                walAddr `shouldBe` genAddr

    it "ANY_ADDRESS_POST_13 - Golden tests for script with timelocks" $ \ctx -> do
        let payload = Json [json|{
                "payment": {
                    "all" : [
                        "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                        { "active_from": 120 }
                        ]
                    },
                "stake": {
                    "all" : [
                        "stake_shared_vkh1nac0awgfa4zjsh4elnjmsscz0huhss8q2g0x3n7m539mwaa5m7s",
                        { "active_from": 120 }
                        ]
                    }
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r
        let goldenAddr =
                "addr1xy756z909yycvf5ah8j5pc4cvuedkhvhyylmgfz400t8jdwmwa0hp024gu7dm6h8n252lkgnzemp93mm9kyd48p64mjshqtu3c" :: Text
        validateAddr r goldenAddr

    it "ANY_ADDRESS_POST_14a - at_least 0 is valid when non-validated" $ \ctx -> do
        let payload = Json [json|{
                "payment": {
                    "some": {
                        "from" : [
                            "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                            "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp",
                            "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9"
                            ],
                         "at_least": 0
                         }
                    }
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r

    it "ANY_ADDRESS_POST_14b - at_least 0 is valid when validation is required" $ \ctx -> do
        let payload = Json [json|{
                "payment": {
                    "some": {
                        "from" : [
                            "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                            "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp",
                            "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9"
                            ],
                         "at_least": 0
                         }
                    },
                "validation": "required"
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r

    it "ANY_ADDRESS_POST_14c - at_least 0 is not valid when validation is recommended" $ \ctx -> do
        let payload = Json [json|{
                "payment": {
                    "some": {
                        "from" : [
                            "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                            "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp",
                            "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9"
                            ],
                         "at_least": 0
                         }
                    },
                "validation": "recommended"
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status400 r
        expectErrorMessage errMsg400ScriptWrongCoeffcient r

    it "ANY_ADDRESS_POST_15a - at_least 4 is valid when non-validated" $ \ctx -> do
        let payload = Json [json|{
                "payment": {
                    "some": {
                        "from" : [
                            "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                            "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp",
                            "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9"
                            ],
                         "at_least": 4
                         }
                    }
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r

    it "ANY_ADDRESS_POST_15b - at_least 4 is valid when validation is required" $ \ctx -> do
        let payload = Json [json|{
                "payment": {
                    "some": {
                        "from" : [
                            "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                            "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp",
                            "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9"
                            ],
                         "at_least": 4
                         }
                    },
                "validation": "required"
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status400 r
        expectErrorMessage errMsg400ScriptIllFormed r

    it "ANY_ADDRESS_POST_15c - at_least 4 is not valid when validation is recommended" $ \ctx -> do
        let payload = Json [json|{
                "payment": {
                    "some": {
                        "from" : [
                            "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                            "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp",
                            "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9"
                            ],
                         "at_least": 4
                         }
                    },
                "validation": "recommended"
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status400 r
        expectErrorMessage errMsg400ScriptIllFormed r

    it "ANY_ADDRESS_POST_16a - script with duplicated verification keys is valid when non-validated" $ \ctx -> do
        let payload = Json [json|{
                "payment": {
                    "some": {
                        "from" : [
                            "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                            "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp",
                            "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9"
                            ],
                         "at_least": 2
                         }
                    }
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r

    it "ANY_ADDRESS_POST_16b - script with duplicated verification keys is valid when required validation used" $ \ctx -> do
        let payload = Json [json|{
                "payment": {
                    "some": {
                        "from" : [
                            "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                            "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp",
                            "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9"
                            ],
                         "at_least": 2
                         }
                    },
                "validation": "required"
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r

    it "ANY_ADDRESS_POST_16c - script with duplicated verification keys is invalid when recommended validation used" $ \ctx -> do
        let payload = Json [json|{
                "payment": {
                    "some": {
                        "from" : [
                            "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                            "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                            "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9"
                            ],
                         "at_least": 2
                         }
                    },
                "validation": "recommended"
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status400 r
        expectErrorMessage errMsg400ScriptDuplicateKeys r

    it "ANY_ADDRESS_POST_17a - Script with contradictory timelocks is valid when validation not used" $ \ctx -> do
        let payload = Json [json|{
                "payment": {
                    "all" : [
                        "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                        { "active_from": 120 },
                        { "active_until": 100 }
                        ]
                    },
                "stake": {
                    "all" : [
                        "stake_shared_vkh1nac0awgfa4zjsh4elnjmsscz0huhss8q2g0x3n7m539mwaa5m7s",
                        { "active_from": 120 }
                        ]
                    }
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r

    it "ANY_ADDRESS_POST_17b - Script with contradictory timelocks is invalid when required validation is used" $ \ctx -> do
        let payload = Json [json|{
                "payment": {
                    "all" : [
                        "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                        { "active_from": 120 },
                        { "active_until": 100 }
                        ]
                    },
                "stake": {
                    "all" : [
                        "script_vkh1yf07000d4ml3ywd3d439kmwp07xzgv6p35cwx8h605jfx0dtd4a",
                        { "active_from": 120 }
                        ]
                    },
                "validation": "required"
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r

    it "ANY_ADDRESS_POST_17c - Script with contradictory timelocks is invalid when recommended validation is used" $ \ctx -> do
        let payload = Json [json|{
                "payment": {
                    "all" : [
                        "stake_shared_vkh1nac0awgfa4zjsh4elnjmsscz0huhss8q2g0x3n7m539mwaa5m7s",
                        { "active_from": 120 },
                        { "active_until": 100 }
                        ]
                    },
                "stake": {
                    "all" : [
                        "stake_shared_vkh1nac0awgfa4zjsh4elnjmsscz0huhss8q2g0x3n7m539mwaa5m7s",
                        { "active_from": 120 }
                        ]
                    },
                "validation": "recommended"
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status400 r
        expectErrorMessage errMsg400ScriptTimelocksContradictory r

    it "ANY_ADDRESS_POST_17d - script with mixed payment/delegation verification keys is invalid" $ \ctx -> do
        let payload = Json [json|{
                "payment": {
                    "some": {
                        "from" : [
                            "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                            "stake_shared_vkh1nac0awgfa4zjsh4elnjmsscz0huhss8q2g0x3n7m539mwaa5m7s"
                            ],
                         "at_least": 1
                         }
                    },
                "validation": "required"
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status400 r
        expectErrorMessage errMsg400ScriptNotUniformRoles r

    it "POST_ACCOUNT_01 - Can retrieve account public keys" $ \ctx -> runResourceT $ do
        let initPoolGap = 10
        w <- emptyWalletWith ctx ("Wallet", fixturePassphrase, initPoolGap)

        let endpoint = Link.postAccountKey @'Shelley w (DerivationIndex 0)
        let payload = Json [json|{
                "passphrase": #{fixturePassphrase},
                "format": "extended"
            }|]
        resp <- request @ApiAccountKey ctx endpoint Default payload
        expectErrorMessage errMsg403WrongIndex resp

        -- Request first 10 extended account public keys
        let indices = [0..9]
        accountPublicKeys <- forM indices $ \index -> do
            let accountPath = Link.postAccountKey @'Shelley w (DerivationIndex $ 2147483648 + index)
            let payload1 = Json [json|{
                    "passphrase": #{fixturePassphrase},
                    "format": "extended"
                }|]
            let payload2 = Json [json|{
                    "passphrase": #{fixturePassphrase},
                    "format": "non_extended"
                }|]
            (_, accXPub) <- unsafeRequest @ApiAccountKey ctx accountPath payload1
            (_, accPub) <- unsafeRequest @ApiAccountKey ctx accountPath payload2
            pure [accXPub, accPub]
        length (concat accountPublicKeys) `shouldBe` 20
  where
    validateAddr resp expected = do
        let addr = getFromResponse id resp
        toJSON addr `shouldBe` object ["address" .= expected ]
