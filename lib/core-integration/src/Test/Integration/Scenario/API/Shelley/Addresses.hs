{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
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
    , ApiAddress
    , ApiTransaction
    , ApiVerificationKey
    , ApiWallet
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( AccountingStyle (..), DerivationIndex (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( defaultAddressPoolGap, getAddressPoolGap )
import Cardano.Wallet.Primitive.Types.Address
    ( AddressState (..) )
import Control.Monad
    ( forM, forM_ )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Aeson
    ( ToJSON (..), object, (.=) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Test.Hspec
    ( SpecWith, describe, shouldBe, shouldNotSatisfy, shouldSatisfy )
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
    , fixtureWallet
    , getFromResponse
    , json
    , listAddresses
    , minUTxOValue
    , request
    , unsafeRequest
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg404NoWallet )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as Aeson
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP
import qualified Test.Hspec.Expectations.Lifted as Expectations

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

    it "ADDRESS_LIST_01 - Can list addresses with non-default pool gap" $ \ctx -> runResourceT $ do
        let g = 15
        w <- emptyWalletWith ctx ("Wallet", "cardano-wallet", g)
        r <- request @[ApiAddress n] ctx
            (Link.listAddresses @'Shelley w) Default Empty
        expectResponseCode HTTP.status200 r
        expectListSize g r
        forM_ [0..(g-1)] $ \addrNum -> do
            expectListField addrNum (#state . #getApiT) (`shouldBe` Unused) r

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
        wDest <- emptyWalletWith ctx ("Wallet", "cardano-wallet", initPoolGap)

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
                    "passphrase": "cardano-wallet"
                }|]

            rTrans <- request @(ApiTransaction n) ctx
                (Link.createTransaction @'Shelley wSrc) Default payload
            expectResponseCode HTTP.status202 rTrans

        -- make sure all transactions are in ledger
        eventually "Wallet balance = initPoolGap * minUTxOValue" $ do
            rb <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            expectField
                (#balance . #getApiT . #available)
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

    it "ADDRESS_INSPECT_01 - Address inspect OK" $ \ctx -> do
        let str = "Ae2tdPwUPEYz6ExfbWubiXPB6daUuhJxikMEb4eXRp5oKZBKZwrbJ2k7EZe"
        r <- request @Aeson.Value ctx (Link.inspectAddress str) Default Empty
        expectResponseCode HTTP.status200 r

    it "ADDRESS_INSPECT_02 - Address inspect KO" $ \ctx -> runResourceT $ do
        let str = "patate"
        r <- request @Aeson.Value ctx (Link.inspectAddress str) Default Empty
        expectResponseCode HTTP.status400 r

    -- Generating golden test data for enterprise addresses - script credential:
    --- $ cardano-address script hash "$(cat script.txt)" \
    --- | cardano-address address payment --from-script --network-tag mainnet
    it "ANY_ADDRESS_POST_01 - Golden tests for enterprise script address - signature" $ \ctx -> do
        --- $ cat script.txt
        --- script_vkh1yf07000d4ml3ywd3d439kmwp07xzgv6p35cwx8h605jfx0dtd4a
        let payload = Json [json|{
                "payment": "script_vkh1yf07000d4ml3ywd3d439kmwp07xzgv6p35cwx8h605jfx0dtd4a"
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r
        let goldenAddr =
                "addr1w96eswctz5wzrv3ceh3h4y3na2t6d95sjn23dawy0zlzg0q0j39eu" :: Text
        validateAddr r goldenAddr

    it "ANY_ADDRESS_POST_02 - Golden tests for enterprise script address - any" $ \ctx -> do
        --- $ cat script.txt
        --- any [script_vkh1yf07000d4ml3ywd3d439kmwp07xzgv6p35cwx8h605jfx0dtd4a, script_vkh1mwlngj4fcwegw53tdmyemfupen2758xwvudmcz9ap8cnqk7jmh4]
        let payload = Json [json|{
                "payment": {
                    "any": [
                        "script_vkh1yf07000d4ml3ywd3d439kmwp07xzgv6p35cwx8h605jfx0dtd4a",
                        "script_vkh1mwlngj4fcwegw53tdmyemfupen2758xwvudmcz9ap8cnqk7jmh4"
                        ]
                    }
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r
        let goldenAddr =
                "addr1wxt2z3pa7etaxp7jurdg0m8jhsmtp4r2z56pd3a5q3jhxyc3vza6h" :: Text
        validateAddr r goldenAddr

    it "ANY_ADDRESS_POST_03 - Golden tests for enterprise script address - all" $ \ctx -> do
        --- $ cat script.txt
        --- all [script_vkh1yf07000d4ml3ywd3d439kmwp07xzgv6p35cwx8h605jfx0dtd4a, script_vkh1mwlngj4fcwegw53tdmyemfupen2758xwvudmcz9ap8cnqk7jmh4]
        let payload = Json [json|{
                "payment": {
                    "all": [
                        "script_vkh1yf07000d4ml3ywd3d439kmwp07xzgv6p35cwx8h605jfx0dtd4a",
                        "script_vkh1mwlngj4fcwegw53tdmyemfupen2758xwvudmcz9ap8cnqk7jmh4"
                        ]
                    }
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r
        let goldenAddr =
                "addr1w94h4mtdkxr2x68zx4tk0cgmd9hymjgsuhmzaxkg5tkl3scc0g8xj" :: Text
        validateAddr r goldenAddr

    it "ANY_ADDRESS_POST_04 - Golden tests for enterprise script address - some" $ \ctx -> do
        --- $ cat script.txt
        --- at_least 2 [script_vkh1yf07000d4ml3ywd3d439kmwp07xzgv6p35cwx8h605jfx0dtd4a,script_vkh1mwlngj4fcwegw53tdmyemfupen2758xwvudmcz9ap8cnqk7jmh4,script_vkh1qw4l62k4203dllrk3dk3sfjpnh3gufhtrtm4qvtrvn4xjp5x5rt]
        let payload = Json [json|{
                "payment": {
                    "some": {
                        "from" : [
                            "script_vkh1yf07000d4ml3ywd3d439kmwp07xzgv6p35cwx8h605jfx0dtd4a",
                            "script_vkh1mwlngj4fcwegw53tdmyemfupen2758xwvudmcz9ap8cnqk7jmh4",
                            "script_vkh1qw4l62k4203dllrk3dk3sfjpnh3gufhtrtm4qvtrvn4xjp5x5rt"
                            ],
                         "at_least": 2
                         }
                    }
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r
        let goldenAddr =
                "addr1wy5np0m5x03tax3kcdh6e2cet98qcfs80wtv4cyvl5taclc6dnd8e" :: Text
        validateAddr r goldenAddr

    -- Generating golden test data for reward account addresses - script credential:
    --- $ cardano-address script hash "$(cat script.txt)" \
    --- | cardano-address address stake --from-script --network-tag mainnet
    it "ANY_ADDRESS_POST_05 - Golden tests for reward account script address - any" $ \ctx -> do
        let payload = Json [json|{
                "stake": {
                    "any": [
                        "script_vkh1yf07000d4ml3ywd3d439kmwp07xzgv6p35cwx8h605jfx0dtd4a",
                        "script_vkh1mwlngj4fcwegw53tdmyemfupen2758xwvudmcz9ap8cnqk7jmh4"
                        ]
                    }
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r
        let goldenAddr =
                "stake17xt2z3pa7etaxp7jurdg0m8jhsmtp4r2z56pd3a5q3jhxycdxzmx9" :: Text
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
                            "script_vkh1yf07000d4ml3ywd3d439kmwp07xzgv6p35cwx8h605jfx0dtd4a",
                            "script_vkh1mwlngj4fcwegw53tdmyemfupen2758xwvudmcz9ap8cnqk7jmh4",
                            "script_vkh1qw4l62k4203dllrk3dk3sfjpnh3gufhtrtm4qvtrvn4xjp5x5rt"
                            ],
                         "at_least": 2
                         }
                    },
                "stake": {
                    "any": [
                        "script_vkh1yf07000d4ml3ywd3d439kmwp07xzgv6p35cwx8h605jfx0dtd4a",
                        "script_vkh1mwlngj4fcwegw53tdmyemfupen2758xwvudmcz9ap8cnqk7jmh4"
                        ]
                    }
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r
        let goldenAddr =
                "addr1xy5np0m5x03tax3kcdh6e2cet98qcfs80wtv4cyvl5tacluk59zr\
                \majh6vra9cx6slk090pkkr2x59f5zmrmgpr9wvfs37hjk4" :: Text
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
                            "script_vkh1yf07000d4ml3ywd3d439kmwp07xzgv6p35cwx8h605jfx0dtd4a",
                            "script_vkh1mwlngj4fcwegw53tdmyemfupen2758xwvudmcz9ap8cnqk7jmh4",
                            "script_vkh1qw4l62k4203dllrk3dk3sfjpnh3gufhtrtm4qvtrvn4xjp5x5rt"
                            ],
                         "at_least": 2
                         }
                     },
                "stake": "stake_vk16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7qau558d"
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r
        let goldenAddr =
                "addr1zy5np0m5x03tax3kcdh6e2cet98qcfs80wtv4cyvl5tacle5rh7cflza8\
                \t3m5wlaj45sg53nvtwpc73mqk90ghv7vv7sleajnd" :: Text
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
                            "script_vkh1yf07000d4ml3ywd3d439kmwp07xzgv6p35cwx8h605jfx0dtd4a",
                            "script_vkh1mwlngj4fcwegw53tdmyemfupen2758xwvudmcz9ap8cnqk7jmh4",
                            "script_vkh1qw4l62k4203dllrk3dk3sfjpnh3gufhtrtm4qvtrvn4xjp5x5rt"
                            ],
                         "at_least": 2
                         }
                    }
            }|]
        r <- request @AnyAddress ctx Link.postAnyAddress Default payload
        expectResponseCode HTTP.status202 r
        let goldenAddr =
                "addr1y9qthemrg5kczwfjjnahwt65elhrl95e9hcgufnajtp6wfffxzlhgvlzh\
                \6drdsm04j43jk2wpsnqw7uketsgelghm3lsch4g8t" :: Text
        validateAddr r goldenAddr

    it "ANY_ADDRESS_POST_12 - Delegating addresses API roundtrip" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx

        -- Generate first 20 addresses using payment and stake keys derived from
        -- wallet API
        let indices = [0..19]
        generatedAddresses <- forM indices $ \index -> do
            let paymentPath = Link.getWalletKey w UtxoExternal (DerivationIndex index)
            (_, paymentKey) <- unsafeRequest @ApiVerificationKey ctx paymentPath Empty

            let stakePath = Link.getWalletKey w MutableAccount (DerivationIndex 0)
            (_, stakeKey) <- unsafeRequest @ApiVerificationKey ctx stakePath Empty

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
                walAddr `Expectations.shouldBe` genAddr

    it "ANY_ADDRESS_POST_13 - at_least must make sense" $ \ctx -> do
        forM_ ([0, 4, 333] :: [Int]) $ \atLeast -> do
            let payload = Json [json|{
                    "payment": {
                        "some": {
                            "from" : [
                                "script_vkh1yf07000d4ml3ywd3d439kmwp07xzgv6p35cwx8h605jfx0dtd4a",
                                "script_vkh1mwlngj4fcwegw53tdmyemfupen2758xwvudmcz9ap8cnqk7jmh4",
                                "script_vkh1qw4l62k4203dllrk3dk3sfjpnh3gufhtrtm4qvtrvn4xjp5x5rt"
                                ],
                             "at_least": #{atLeast}
                             }
                        }
                }|]
            r <- request @AnyAddress ctx Link.postAnyAddress Default payload
            expectResponseCode HTTP.status400 r
            expectErrorMessage "must have at least one credential" r
  where
    validateAddr resp expected = do
        let addr = getFromResponse id resp
        toJSON addr `shouldBe` object ["address" .= expected ]
