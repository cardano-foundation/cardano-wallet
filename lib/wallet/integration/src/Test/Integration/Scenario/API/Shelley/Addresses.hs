{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Integration.Scenario.API.Shelley.Addresses
  ( spec
  )
where

import Cardano.Wallet.Address.Derivation
  ( DerivationIndex (..)
  , Role (..)
  )
import Cardano.Wallet.Address.Discovery.Sequential
  ( defaultAddressPoolGap
  , getAddressPoolGap
  , purposeCIP1852
  )
import Cardano.Wallet.Api.Link qualified as Link
import Cardano.Wallet.Api.Types
  ( AnyAddress
  , ApiAccountKey
  , ApiAddressWithPath
  , ApiT (..)
  , ApiTransaction
  , ApiVerificationKeyShelley
  , ApiWallet
  , WalletStyle (..)
  )
import Cardano.Wallet.Primitive.NetworkId
  ( HasSNetworkId
  )
import Cardano.Wallet.Primitive.Types.Address
  ( AddressState (..)
  )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
  ( TxStatus (..)
  )
import Control.Monad
  ( forM
  , forM_
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Control.Monad.Trans.Resource
  ( runResourceT
  )
import Data.Aeson
  ( ToJSON (..)
  , object
  , (.=)
  )
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens qualified as Aeson
import Data.Generics.Internal.VL.Lens
  ( view
  , (^.)
  )
import Data.Quantity
  ( Quantity (..)
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as T
import Network.HTTP.Types.Status qualified as HTTP
import Test.Hspec
  ( SpecWith
  , describe
  )
import Test.Hspec.Expectations.Lifted
  ( shouldBe
  , shouldNotBe
  , shouldNotSatisfy
  , shouldSatisfy
  )
import Test.Hspec.Extra
  ( it
  )
import Test.Integration.Framework.DSL
  ( Context (..)
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
import Prelude

spec
  :: forall n
   . HasSNetworkId n
  => SpecWith Context
spec = describe "SHELLEY_ADDRESSES" $ do
  it "BYRON_ADDRESS_LIST - Byron wallet on Shelley ep" $ \ctx -> runResourceT $ do
    w <- emptyRandomWallet ctx
    let
      wid = w ^. walletId
    let
      ep = ("GET", "v2/wallets/" <> wid <> "/addresses")
    r <- request @[ApiAddressWithPath n] ctx ep Default Empty
    expectResponseCode HTTP.status404 r
    expectErrorMessage (errMsg404NoWallet wid) r

  it "ADDRESS_LIST_01 - Can list known addresses on a default wallet" $ \ctx -> runResourceT $ do
    let
      g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
    w <- emptyWallet ctx
    r <-
      request @[ApiAddressWithPath n]
        ctx
        (Link.listAddresses @'Shelley w)
        Default
        Empty
    expectResponseCode HTTP.status200 r
    expectListSize g r
    forM_ [0 .. (g - 1)] $ \addrNum -> do
      expectListField addrNum (#state . #getApiT) (`shouldBe` Unused) r
      expectListField
        addrNum
        #derivationPath
        (`shouldSatisfy` (isValidDerivationPath purposeCIP1852))
        r

  it "ADDRESS_LIST_01 - Can list addresses with non-default pool gap" $ \ctx -> runResourceT $ do
    let
      g = 15
    w <- emptyWalletWith ctx ("Wallet", fixturePassphrase, g)
    r <-
      request @[ApiAddressWithPath n]
        ctx
        (Link.listAddresses @'Shelley w)
        Default
        Empty
    expectResponseCode HTTP.status200 r
    expectListSize g r
    forM_ [0 .. (g - 1)] $ \addrNum -> do
      expectListField addrNum (#state . #getApiT) (`shouldBe` Unused) r
      expectListField
        addrNum
        #derivationPath
        (`shouldSatisfy` (isValidDerivationPath purposeCIP1852))
        r

  it "ADDRESS_LIST_02 - Can filter used and unused addresses" $ \ctx -> runResourceT $ do
    let
      g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
    w <- fixtureWallet ctx
    rUsed <-
      request @[ApiAddressWithPath n]
        ctx
        (Link.listAddresses' @'Shelley w (Just Used))
        Default
        Empty
    expectResponseCode HTTP.status200 rUsed
    expectListSize 10 rUsed
    forM_ [0 .. 9] $ \addrNum -> do
      expectListField
        addrNum
        (#state . #getApiT)
        (`shouldBe` Used)
        rUsed
    rUnused <-
      request @[ApiAddressWithPath n]
        ctx
        (Link.listAddresses' @'Shelley w (Just Unused))
        Default
        Empty
    expectResponseCode HTTP.status200 rUnused
    expectListSize g rUnused
    forM_ [10 .. (g - 1)] $ \addrNum -> do
      expectListField
        addrNum
        (#state . #getApiT)
        (`shouldBe` Unused)
        rUnused

  it "ADDRESS_LIST_02 - Shows nothing when there are no used addresses"
    $ \ctx -> runResourceT $ do
      w <- emptyWallet ctx
      rUsed <-
        request @[ApiAddressWithPath n]
          ctx
          (Link.listAddresses' @'Shelley w (Just Used))
          Default
          Empty
      rUnused <-
        request @[ApiAddressWithPath n]
          ctx
          (Link.listAddresses' @'Shelley w (Just Unused))
          Default
          Empty
      expectResponseCode HTTP.status200 rUsed
      expectListSize 0 rUsed
      expectResponseCode HTTP.status200 rUnused
      expectListSize 20 rUnused
      forM_ [0 .. 19] $ \addrNum -> do
        expectListField
          addrNum
          (#state . #getApiT)
          (`shouldBe` Unused)
          rUnused

  -- TODO
  -- MOVE TO test/unit/Cardano/Wallet/ApiSpec.hs
  describe "ADDRESS_LIST_02 - Invalid filters are bad requests" $ do
    let
      filters =
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

    let
      withQuery f (method, link) = (method, link <> "?state=" <> T.pack f)
    forM_ filters $ \fil -> it fil $ \ctx -> runResourceT $ do
      w <- emptyWallet ctx
      let
        link = withQuery fil $ Link.listAddresses @'Shelley w
      r <- request @[ApiAddressWithPath n] ctx link Default Empty
      verify
        r
        [ expectResponseCode HTTP.status400
        , expectErrorMessage
            "Error parsing query parameter state failed: Unable to\
            \ decode the given text value. Please specify\
            \ one of the following values: used, unused."
        ]

  it "ADDRESS_LIST_03 - Generates new address pool gap" $ \ctx -> runResourceT $ do
    let
      initPoolGap = 10
    wSrc <- fixtureWallet ctx
    wDest <- emptyWalletWith ctx ("Wallet", fixturePassphrase, initPoolGap)

    -- make sure all addresses in address_pool_gap are 'Unused'
    r <-
      request @[ApiAddressWithPath n]
        ctx
        (Link.listAddresses @'Shelley wDest)
        Default
        Empty
    verify
      r
      [ expectResponseCode HTTP.status200
      , expectListSize initPoolGap
      ]
    forM_ [0 .. 9] $ \addrNum -> do
      expectListField addrNum (#state . #getApiT) (`shouldBe` Unused) r
    addrs <- listAddresses @n ctx wDest

    let
      amt = minUTxOValue (_mainEra ctx)

    -- run 10 transactions to make all addresses `Used`
    forM_ [0 .. 9] $ \addrNum -> do
      let
        destination = (addrs !! addrNum) ^. #id
      let
        payload =
          Json
            [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": #{amt},
                            "unit": "lovelace"
                        }
                    }],
                    "passphrase": #{fixturePassphrase}
                }|]

      rTrans <-
        request @(ApiTransaction n)
          ctx
          (Link.createTransactionOld @'Shelley wSrc)
          Default
          payload
      expectResponseCode HTTP.status202 rTrans

    -- make sure all transactions are in ledger
    eventually "Wallet balance = initPoolGap * minUTxOValue" $ do
      rb <-
        request @ApiWallet
          ctx
          (Link.getWallet @'Shelley wDest)
          Default
          Empty
      expectField
        (#balance . #available)
        (`shouldBe` Quantity (10 * amt))
        rb

    -- verify new address_pool_gap has been created
    rAddr <-
      request @[ApiAddressWithPath n]
        ctx
        (Link.listAddresses @'Shelley wDest)
        Default
        Empty
    verify
      rAddr
      [ expectResponseCode HTTP.status200
      , expectListSize 20
      ]
    forM_ [0 .. 9] $ \addrNum -> do
      expectListField
        addrNum
        (#state . #getApiT)
        (`shouldBe` Used)
        rAddr
    forM_ [10 .. 19] $ \addrNum -> do
      expectListField
        addrNum
        (#state . #getApiT)
        (`shouldBe` Unused)
        rAddr

  it "ADDRESS_LIST_04 - Deleted wallet" $ \ctx -> runResourceT $ do
    w <- emptyWallet ctx
    _ <-
      request @ApiWallet
        ctx
        (Link.deleteWallet @'Shelley w)
        Default
        Empty
    r <-
      request @[ApiAddressWithPath n]
        ctx
        (Link.listAddresses @'Shelley w)
        Default
        Empty
    expectResponseCode HTTP.status404 r
    expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

  it "ADDRESS_LIST_05 - bech32 HRP is correct - mainnet" $ \ctx -> runResourceT $ do
    w <- emptyWallet ctx
    r <-
      request @[Aeson.Value]
        ctx
        (Link.listAddresses @'Shelley w)
        Default
        Empty
    verify
      r
      [ expectResponseCode HTTP.status200
      , -- integration tests are configured for mainnet
        expectListField
          0
          (Aeson.key "id" . Aeson._String)
          (`shouldSatisfy` T.isPrefixOf "addr")
      , expectListField
          0
          (Aeson.key "id" . Aeson._String)
          (`shouldNotSatisfy` T.isPrefixOf "addr_test")
      ]

  it
    "ADDRESS_LIST_06 - Used change addresses are listed after a transaction is no longer pending" $ \ctx -> runResourceT @IO $ do
    let
      verifyAddrs nTotal nUsed addrs = do
        liftIO (length addrs `shouldBe` nTotal)
        let
          onlyUsed = filter ((== Used) . (^. (#state . #getApiT))) addrs
        liftIO (length onlyUsed `shouldBe` nUsed)

    -- 1. Create Shelley wallets
    let
      initialTotalA = 30
    let
      initialUsedA = 10
    wA <- fixtureWallet ctx
    listAddresses @n ctx wA
      >>= verifyAddrs initialTotalA initialUsedA

    let
      initialTotalB = 20
    let
      initialUsedB = 0
    wB <- emptyWallet ctx
    listAddresses @n ctx wB
      >>= verifyAddrs initialTotalB initialUsedB

    -- 2. Send a transaction from A -> B
    destination <- view #id . head <$> listAddresses @n ctx wB
    let
      amount = 10 * minUTxOValue (_mainEra ctx)
    let
      payload =
        Json
          [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amount},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{fixturePassphrase}
            }|]
    (_, rtx) <-
      unsafeRequest @(ApiTransaction n)
        ctx
        (Link.createTransactionOld @'Shelley wA)
        payload

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
      request @(ApiTransaction n)
        ctx
        (Link.getTransaction @'Shelley wA rtx)
        Default
        Empty
        >>= expectField #status (`shouldBe` ApiT InLedger)
      request @(ApiTransaction n)
        ctx
        (Link.getTransaction @'Shelley wB rtx)
        Default
        Empty
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

  it "ADDRESS_INSPECT_01 - Address inspect OK Icarus" $ \ctx -> do
    let
      str = "Ae2tdPwUPEYz6ExfbWubiXPB6daUuhJxikMEb4eXRp5oKZBKZwrbJ2k7EZe"
    r <- request @Aeson.Value ctx (Link.inspectAddress str) Default Empty
    verify
      r
      [ expectResponseCode HTTP.status200
      , expectField
          (Aeson.key "address_style" . Aeson._String)
          (`shouldBe` "Icarus")
      , expectField
          (Aeson.key "address_type" . Aeson._Number)
          (`shouldBe` 8)
      ]

  it "ADDRESS_INSPECT_02 - Address inspect OK Byron" $ \ctx -> do
    let
      str =
        "37btjrVyb4KE2ByiPiJUQfAUBGaMyKScg4mnYjzVAsN2PUxj1WxTg98ien3oAo8vKBhP2KTuC9wi76vZ9kDNFkjbmzywdLTJgaz8n3RD3Rueim3Pd3"
    r <- request @Aeson.Value ctx (Link.inspectAddress str) Default Empty
    verify
      r
      [ expectResponseCode HTTP.status200
      , expectField
          (Aeson.key "address_style" . Aeson._String)
          (`shouldBe` "Byron")
      , expectField
          (Aeson.key "address_type" . Aeson._Number)
          (`shouldBe` 8)
      ]

  it "ADDRESS_INSPECT_03 - Address inspect OK reward" $ \ctx -> do
    let
      str = "stake1u8pn5jr7cfa0x8ndtdufyg5lty3avg3zd2tq35c06hpsh8gptdza4"
    r <- request @Aeson.Value ctx (Link.inspectAddress str) Default Empty
    verify
      r
      [ expectResponseCode HTTP.status200
      , expectField
          (Aeson.key "address_style" . Aeson._String)
          (`shouldBe` "Shelley")
      , expectField
          (Aeson.key "address_type" . Aeson._Number)
          (`shouldBe` 14)
      ]

  it "ADDRESS_INSPECT_04 - Address inspect KO" $ \ctx -> runResourceT $ do
    let
      str = "patate"
    r <- request @Aeson.Value ctx (Link.inspectAddress str) Default Empty
    expectResponseCode HTTP.status400 r

  it "ADDRESS_INSPECT_05 - Address inspect OK bech32" $ \ctx -> do
    let
      str =
        "addr_test1qzamu40sglnsrylzv9jylekjmzgaqsg5v5z9u6yk3jpnnxjwck77fqu8deuumsvnazjnjhwasc2eetfqpa2pvygts78ssd5388"
    r <- request @Aeson.Value ctx (Link.inspectAddress str) Default Empty
    verify
      r
      [ expectResponseCode HTTP.status200
      , expectField
          (Aeson.key "address_style" . Aeson._String)
          (`shouldBe` "Shelley")
      , expectField
          (Aeson.key "spending_key_hash_bech32" . Aeson._String)
          (`shouldBe` "addr_vkh1hwl9tuz8uuqe8cnpv387d5kcj8gyz9r9q30x395vsvue5e44fh7")
      , expectField
          (Aeson.key "stake_key_hash_bech32" . Aeson._String)
          (`shouldBe` "stake_vkh1fmzmmeyrsah8nnwpj0522w2amkrpt89dyq84g9s3pwrc7dqjnfu")
      , expectField
          (Aeson.key "address_type" . Aeson._Number)
          (`shouldBe` 0)
      ]

  -- Generating golden test data for enterprise addresses - script credential:
  --- (a) from script hash
  --- $ cardano-address script hash "$(cat script.txt)" \
  --- | cardano-address address payment --network-tag mainnet
  --- (b) from script
  --- $ cardano-address address payment --network-tag mainnet "$(cat script.txt)"
  it
    "ANY_ADDRESS_POST_01 - Golden tests for enterprise script address - signature" $ \ctx -> do
    --- $ cat script.txt
    --- addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq
    --- $ cardano-address script hash "$(cat script.txt)"
    --- script1ccqe6wa40878s2pxrfwj0qxz9t7dxw8rhfreqwzjuy67gk2ausz
    let
      payload1 =
        Json
          [json|{
                "payment": "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq"
            }|]
    r1 <- request @AnyAddress ctx Link.postAnyAddress Default payload1
    expectResponseCode HTTP.status202 r1
    let
      payload2 =
        Json
          [json|{
                "payment": "script1ccqe6wa40878s2pxrfwj0qxz9t7dxw8rhfreqwzjuy67gk2ausz"
            }|]
    r2 <- request @AnyAddress ctx Link.postAnyAddress Default payload2
    expectResponseCode HTTP.status202 r2
    let
      goldenAddr =
        "addr1w8rqr8fmk4ulc7pgycd96fuqcg40e5ecuway0ypc2tsnteqm5wul2" :: Text
    validateAddr r1 goldenAddr
    validateAddr r2 goldenAddr

  it "ANY_ADDRESS_POST_02 - Golden tests for enterprise script address - any" $ \ctx -> do
    --- $ cat script.txt
    --- any [addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq, addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp]
    --- $ cardano-address script hash "$(cat script.txt)"
    --- script1ujl6y7gx0e3h79kyzqan0smw3xq6x289za64fn6tap6xc7rsm0z
    let
      payload1 =
        Json
          [json|{
                "payment": {
                    "any": [
                        "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                        "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp"
                        ]
                    }
            }|]
    r1 <- request @AnyAddress ctx Link.postAnyAddress Default payload1
    expectResponseCode HTTP.status202 r1
    let
      payload2 =
        Json
          [json|{
                "payment": "script1ujl6y7gx0e3h79kyzqan0smw3xq6x289za64fn6tap6xc7rsm0z"
            }|]
    r2 <- request @AnyAddress ctx Link.postAnyAddress Default payload2
    expectResponseCode HTTP.status202 r2
    let
      goldenAddr =
        "addr1w8jtlgneqelxxlckcsgrkd7rd6ycrgegu5th24x0f058gmqhsnv92" :: Text
    validateAddr r1 goldenAddr
    validateAddr r2 goldenAddr

  it "ANY_ADDRESS_POST_03 - Golden tests for enterprise script address - all" $ \ctx -> do
    --- $ cat script.txt
    --- all [addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq, addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp]
    --- $ cardano-address script hash "$(cat script.txt)"
    --- script1gr69m385thgvkrtspk73zmkwk537wxyxuevs2u9cukglvtlkz4k
    let
      payload1 =
        Json
          [json|{
                "payment": {
                    "all": [
                        "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                        "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp"
                        ]
                    }
            }|]
    r1 <- request @AnyAddress ctx Link.postAnyAddress Default payload1
    expectResponseCode HTTP.status202 r1
    let
      payload2 =
        Json
          [json|{
                "payment": "script1gr69m385thgvkrtspk73zmkwk537wxyxuevs2u9cukglvtlkz4k"
            }|]
    r2 <- request @AnyAddress ctx Link.postAnyAddress Default payload2
    expectResponseCode HTTP.status202 r2
    let
      goldenAddr =
        "addr1w9q0ghwy73wapjcdwqxm6ytwe66j8eccsmn9jptshrjerasvf2cg0" :: Text
    validateAddr r1 goldenAddr
    validateAddr r2 goldenAddr

  it "ANY_ADDRESS_POST_04 - Golden tests for enterprise script address - some" $ \ctx -> do
    --- $ cat script.txt
    --- at_least 2 [addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq,addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp,addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9]
    --- $ cardano-address script hash "$(cat script.txt)"
    ---- script1qxu7mh9eaxt6fh2z87hwz46wgy7z8kjyqmlmfcrvnfa02aj9778
    let
      payload1 =
        Json
          [json|{
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
    r1 <- request @AnyAddress ctx Link.postAnyAddress Default payload1
    expectResponseCode HTTP.status202 r1
    let
      payload2 =
        Json
          [json|{
                "payment": "script1qxu7mh9eaxt6fh2z87hwz46wgy7z8kjyqmlmfcrvnfa02aj9778"
            }|]
    r2 <- request @AnyAddress ctx Link.postAnyAddress Default payload2
    expectResponseCode HTTP.status202 r2
    let
      goldenAddr =
        "addr1wyqmnmwuh85e0fxaggl6ac2hfeqncg76gsr0ld8qdjd84ag6sm0n8" :: Text
    validateAddr r1 goldenAddr
    validateAddr r2 goldenAddr

  -- Generating golden test data for reward account addresses - script credential:
  --- (a) script hash
  --- $ cardano-address script hash "$(cat script.txt)" \
  --- | cardano-address address stake --network-tag mainnet
  --- (b) script
  --- $ cardano-address address stake --network-tag mainnet "$(cat script.txt)"
  it "ANY_ADDRESS_POST_05 - Golden tests for reward account script address - any" $ \ctx -> do
    --- $ cat script.txt
    --- any [stake_shared_vkh1nqc00hvlc6cq0sfhretk0rmzw8dywmusp8retuqnnxzajtzhjg5, stake_shared_vkh1nac0awgfa4zjsh4elnjmsscz0huhss8q2g0x3n7m539mwaa5m7s]
    --- $ cardano-address script hash "$(cat script.txt)"
    --- script1y9c2v4j9efmhxmyuefyfzd7t8lcdfra0x3pagy20ekrpvfxxxyz
    let
      payload1 =
        Json
          [json|{
                "stake": {
                    "any": [
                        "stake_shared_vkh1nqc00hvlc6cq0sfhretk0rmzw8dywmusp8retuqnnxzajtzhjg5",
                        "stake_shared_vkh1nac0awgfa4zjsh4elnjmsscz0huhss8q2g0x3n7m539mwaa5m7s"
                        ]
                    }
            }|]
    r1 <- request @AnyAddress ctx Link.postAnyAddress Default payload1
    expectResponseCode HTTP.status202 r1
    let
      payload2 =
        Json
          [json|{
                "stake": "script1y9c2v4j9efmhxmyuefyfzd7t8lcdfra0x3pagy20ekrpvfxxxyz"
            }|]
    r2 <- request @AnyAddress ctx Link.postAnyAddress Default payload2
    expectResponseCode HTTP.status202 r2
    let
      goldenAddr =
        "stake17yshpfjkgh98wumvnn9y3yfhevllp4y04u6y84q3flxcv9sduxphm" :: Text
    validateAddr r1 goldenAddr
    validateAddr r2 goldenAddr

  -- Generating golden test data for reward account addresses - both script credentials:
  --- (a) script hashes
  --- $ cardano-address script hash "$(cat script1.txt)" \
  --- | cardano-address address payment --network-tag mainnet \
  --- | cardano-address address delegation $(cardano-address script hash "$(cat script2.txt)")
  --- (b) scripts
  --- $ cardano-address address payment --network-tag mainnet "$(cat script1.txt)" \
  --- | cardano-address address delegation "$(cat script2.txt)"

  --- $ cat script1.txt
  --- at_least 2 [addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq,addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp,addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9]
  --- $ cardano-address script hash "$(cat script1.txt)"
  --- script1qxu7mh9eaxt6fh2z87hwz46wgy7z8kjyqmlmfcrvnfa02aj9778
  --- $ cat script2.txt
  --- any [stake_shared_vkh1nqc00hvlc6cq0sfhretk0rmzw8dywmusp8retuqnnxzajtzhjg5, stake_shared_vkh1nac0awgfa4zjsh4elnjmsscz0huhss8q2g0x3n7m539mwaa5m7s]
  --- $ cardano-address script hash "$(cat script2.txt)"
  --- script1y9c2v4j9efmhxmyuefyfzd7t8lcdfra0x3pagy20ekrpvfxxxyz
  it "ANY_ADDRESS_POST_06 - Golden tests for delegating script address - any" $ \ctx -> do
    let
      payload1 =
        Json
          [json|{
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
    r1 <- request @AnyAddress ctx Link.postAnyAddress Default payload1
    expectResponseCode HTTP.status202 r1
    let
      payload2 =
        Json
          [json|{
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
                "stake": "script1y9c2v4j9efmhxmyuefyfzd7t8lcdfra0x3pagy20ekrpvfxxxyz"
            }|]
    r2 <- request @AnyAddress ctx Link.postAnyAddress Default payload2
    expectResponseCode HTTP.status202 r2
    let
      payload3 =
        Json
          [json|{
                "payment": "script1qxu7mh9eaxt6fh2z87hwz46wgy7z8kjyqmlmfcrvnfa02aj9778",
                "stake": {
                    "any": [
                        "stake_shared_vkh1nqc00hvlc6cq0sfhretk0rmzw8dywmusp8retuqnnxzajtzhjg5",
                        "stake_shared_vkh1nac0awgfa4zjsh4elnjmsscz0huhss8q2g0x3n7m539mwaa5m7s"
                        ]
                    }
            }|]
    r3 <- request @AnyAddress ctx Link.postAnyAddress Default payload3
    expectResponseCode HTTP.status202 r3
    let
      payload4 =
        Json
          [json|{
                "payment": "script1qxu7mh9eaxt6fh2z87hwz46wgy7z8kjyqmlmfcrvnfa02aj9778",
                "stake": "script1y9c2v4j9efmhxmyuefyfzd7t8lcdfra0x3pagy20ekrpvfxxxyz"
            }|]
    r4 <- request @AnyAddress ctx Link.postAnyAddress Default payload4
    expectResponseCode HTTP.status202 r4

    let
      goldenAddr =
        "addr1xyqmnmwuh85e0fxaggl6ac2hfeqncg76gsr0ld8qdjd84afpwzn9v3w2waeke8x2fzgn0jel7r2glte5g02pzn7dsctqu6mtx3"
          :: Text
    validateAddr r1 goldenAddr
    validateAddr r2 goldenAddr
    validateAddr r3 goldenAddr
    validateAddr r4 goldenAddr

  -- Generating golden test. We use the following mnemonic in all examples below:
  --- $ cat recovery-phrase.txt
  --- east student silly already breeze enact seat trade few way online skin grass humble electric

  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/0/0 \
  --- > | cardano-address key public --without-chain-code
  --- addr_vk1lqglg77z6kajsdz4739q22c0zm0yhuy567z6xk2vc0z5ucjtkwpschzd2j
  -- Golden address can be obtained via
  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/0/0 \
  --- > | cardano-address key public --without-chain-code \
  --- > | cardano-address address payment --network-tag mainnet
  it
    "ANY_ADDRESS_POST_07a - Golden tests for enterprise address - from non-extended public key" $ \ctx -> do
    let
      payload =
        Json
          [json|{
                "payment": "addr_vk1lqglg77z6kajsdz4739q22c0zm0yhuy567z6xk2vc0z5ucjtkwpschzd2j"
            }|]
    r <- request @AnyAddress ctx Link.postAnyAddress Default payload
    expectResponseCode HTTP.status202 r
    let
      goldenAddr =
        "addr1v9qthemrg5kczwfjjnahwt65elhrl95e9hcgufnajtp6wfgknj82e" :: Text
    validateAddr r goldenAddr

  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/0/0 \
  --- > | cardano-address key public --with-chain-code
  --- addr_xvk1lqglg77z6kajsdz4739q22c0zm0yhuy567z6xk2vc0z5ucjtkwp3k2zz8796vdstcu7q0qp232wyvzjes0qkpmt7gzwa0x2q75h3qcgl5y4q0
  -- Golden address can be obtained via
  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/0/0 \
  --- > | cardano-address key public --with-chain-code \
  --- > | cardano-address address payment --network-tag mainnet
  it
    "ANY_ADDRESS_POST_07b - Golden tests for enterprise address - from extended public key" $ \ctx -> do
    let
      payload =
        Json
          [json|{
                "payment": "addr_xvk1lqglg77z6kajsdz4739q22c0zm0yhuy567z6xk2vc0z5ucjtkwp3k2zz8796vdstcu7q0qp232wyvzjes0qkpmt7gzwa0x2q75h3qcgl5y4q0"
            }|]
    r <- request @AnyAddress ctx Link.postAnyAddress Default payload
    expectResponseCode HTTP.status202 r
    let
      goldenAddr =
        "addr1v9qthemrg5kczwfjjnahwt65elhrl95e9hcgufnajtp6wfgknj82e" :: Text
    validateAddr r goldenAddr

  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/0/0 \
  --- > | cardano-address key public --with-chain-code \
  --- > | cardano-address key hash
  --- addr_vkh1gza7wc699kqnjv55ldmj74x0acledxfd7z8zvlvjcwnj2h09mcs
  --- One can also use --without-chain-code to get the same key hash
  -- Golden address can be obtained via
  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/0/0 \
  --- > | cardano-address key public --with-chain-code \
  --- > | cardano-address key hash
  --- > | cardano-address address payment --network-tag mainnet
  it "ANY_ADDRESS_POST_07c - Golden tests for enterprise address - from key hash" $ \ctx -> do
    let
      payload =
        Json
          [json|{
                "payment": "addr_vkh1gza7wc699kqnjv55ldmj74x0acledxfd7z8zvlvjcwnj2h09mcs"
            }|]
    r <- request @AnyAddress ctx Link.postAnyAddress Default payload
    expectResponseCode HTTP.status202 r
    let
      goldenAddr =
        "addr1v9qthemrg5kczwfjjnahwt65elhrl95e9hcgufnajtp6wfgknj82e" :: Text
    validateAddr r goldenAddr

  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/2/0 \
  --- > | cardano-address key public --without-chain-code
  --- stake_vk16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7qau558d
  -- Golden address can be obtained via
  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/2/0 \
  --- > | cardano-address key public --without-chain-code \
  --- > | cardano-address address stake --network-tag mainnet
  it
    "ANY_ADDRESS_POST_08a - Golden tests for reward account address - from non-extended public key" $ \ctx -> do
    let
      payload =
        Json
          [json|{
                "stake": "stake_vk16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7qau558d"
            }|]
    r <- request @AnyAddress ctx Link.postAnyAddress Default payload
    expectResponseCode HTTP.status202 r
    let
      goldenAddr =
        "stake1uy6pmlvyl3wn4ca6807e26gy2gek9hqu0gastzh5tk0xx0g2rxsr5" :: Text
    validateAddr r goldenAddr

  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/2/0 \
  --- > | cardano-address key public --with-chain-code
  --- stake_xvk16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7yak6lmcyst8yclpm3yalrspc7q2wy9f6683x6f9z4e3gclhs5snslcst62
  -- Golden address can be obtained via
  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/2/0 \
  --- > | cardano-address key public --with-chain-code \
  --- > | cardano-address address stake --network-tag mainnet
  it
    "ANY_ADDRESS_POST_08b - Golden tests for reward account address - from extended public key" $ \ctx -> do
    let
      payload =
        Json
          [json|{
                "stake": "stake_xvk16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7yak6lmcyst8yclpm3yalrspc7q2wy9f6683x6f9z4e3gclhs5snslcst62"
            }|]
    r <- request @AnyAddress ctx Link.postAnyAddress Default payload
    expectResponseCode HTTP.status202 r
    let
      goldenAddr =
        "stake1uy6pmlvyl3wn4ca6807e26gy2gek9hqu0gastzh5tk0xx0g2rxsr5" :: Text
    validateAddr r goldenAddr

  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/2/0 \
  --- > | cardano-address key public --with-chain-code \
  --- > | cardano-address key hash
  --- stake_vkh1xswlmp8ut5aw8w3mlk2kjpzjxd3dc8r68vzc4azane3n6r07ddx
  -- Golden address can be obtained via
  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/2/0 \
  --- > | cardano-address key public --with-chain-code \
  --- > | cardano-address key hash \
  --- > | cardano-address address stake --network-tag mainnet
  it
    "ANY_ADDRESS_POST_08c - Golden tests for reward account address - from key hash" $ \ctx -> do
    let
      payload =
        Json
          [json|{
                "stake": "stake_vkh1xswlmp8ut5aw8w3mlk2kjpzjxd3dc8r68vzc4azane3n6r07ddx"
            }|]
    r <- request @AnyAddress ctx Link.postAnyAddress Default payload
    expectResponseCode HTTP.status202 r
    let
      goldenAddr =
        "stake1uy6pmlvyl3wn4ca6807e26gy2gek9hqu0gastzh5tk0xx0g2rxsr5" :: Text
    validateAddr r goldenAddr

  -- Golden address can be obtained via
  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/2/0 \
  --- > | cardano-address key public --with-chain-code > stake.xvk
  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/2/0 \
  --- > | cardano-address key public --without-chain-code > stake.vk
  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/2/0 \
  --- > | cardano-address key public --with-chain-code \
  --- > | cardano-address key hash > stake.vkh

  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/0/0 \
  --- > | cardano-address key public --with-chain-code \
  --- > | cardano-address address payment --network-tag mainnet \
  --- > | cardano-address address delegation $(cat stake.xvk)
  it
    "ANY_ADDRESS_POST_09a - Golden tests for delegating address with both non-extended pub key credentials" $ \ctx -> do
    let
      payload =
        Json
          [json|{
                "payment": "addr_vk1lqglg77z6kajsdz4739q22c0zm0yhuy567z6xk2vc0z5ucjtkwpschzd2j",
                "stake": "stake_vk16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7qau558d"
            }|]
    r <- request @AnyAddress ctx Link.postAnyAddress Default payload
    expectResponseCode HTTP.status202 r
    let
      goldenAddr =
        "addr1q9qthemrg5kczwfjjnahwt65elhrl95e9hcgufnajtp6wff5r\
        \h7cflza8t3m5wlaj45sg53nvtwpc73mqk90ghv7vv7s64ryn2"
          :: Text
    validateAddr r goldenAddr

  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/0/0 \
  --- > | cardano-address key public --with-chain-code \
  --- > | cardano-address address payment --network-tag mainnet \
  --- > | cardano-address address delegation $(cat stake.xvk)
  it
    "ANY_ADDRESS_POST_09b - Golden tests for delegating address with both extended pub key credentials" $ \ctx -> do
    let
      payload =
        Json
          [json|{
                "payment": "addr_xvk1lqglg77z6kajsdz4739q22c0zm0yhuy567z6xk2vc0z5ucjtkwp3k2zz8796vdstcu7q0qp232wyvzjes0qkpmt7gzwa0x2q75h3qcgl5y4q0",
                "stake": "stake_xvk16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7yak6lmcyst8yclpm3yalrspc7q2wy9f6683x6f9z4e3gclhs5snslcst62"
            }|]
    r <- request @AnyAddress ctx Link.postAnyAddress Default payload
    expectResponseCode HTTP.status202 r
    let
      goldenAddr =
        "addr1q9qthemrg5kczwfjjnahwt65elhrl95e9hcgufnajtp6wff5r\
        \h7cflza8t3m5wlaj45sg53nvtwpc73mqk90ghv7vv7s64ryn2"
          :: Text
    validateAddr r goldenAddr

  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/0/0 \
  --- > | cardano-address key public --with-chain-code \
  --- > | cardano-address key hash \
  --- > | cardano-address address payment --network-tag mainnet \
  --- > | cardano-address address delegation $(cat stake.vkh)
  it
    "ANY_ADDRESS_POST_09c - Golden tests for delegating address with both key hash credentials" $ \ctx -> do
    let
      payload =
        Json
          [json|{
                "payment": "addr_vkh1gza7wc699kqnjv55ldmj74x0acledxfd7z8zvlvjcwnj2h09mcs",
                "stake": "stake_vkh1xswlmp8ut5aw8w3mlk2kjpzjxd3dc8r68vzc4azane3n6r07ddx"
            }|]
    r <- request @AnyAddress ctx Link.postAnyAddress Default payload
    expectResponseCode HTTP.status202 r
    let
      goldenAddr =
        "addr1q9qthemrg5kczwfjjnahwt65elhrl95e9hcgufnajtp6wff5r\
        \h7cflza8t3m5wlaj45sg53nvtwpc73mqk90ghv7vv7s64ryn2"
          :: Text
    validateAddr r goldenAddr

  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/0/0 \
  --- > | cardano-address key public --with-chain-code \
  --- > | cardano-address key hash \
  --- > | cardano-address address payment --network-tag mainnet \
  --- > | cardano-address address delegation $(cat stake.xvk)
  it
    "ANY_ADDRESS_POST_09d - Golden tests for delegating address with mixed credentials" $ \ctx -> do
    let
      payload =
        Json
          [json|{
                "payment": "addr_vkh1gza7wc699kqnjv55ldmj74x0acledxfd7z8zvlvjcwnj2h09mcs",
                "stake": "stake_xvk16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7yak6lmcyst8yclpm3yalrspc7q2wy9f6683x6f9z4e3gclhs5snslcst62"
            }|]
    r <- request @AnyAddress ctx Link.postAnyAddress Default payload
    expectResponseCode HTTP.status202 r
    let
      goldenAddr =
        "addr1q9qthemrg5kczwfjjnahwt65elhrl95e9hcgufnajtp6wff5r\
        \h7cflza8t3m5wlaj45sg53nvtwpc73mqk90ghv7vv7s64ryn2"
          :: Text
    validateAddr r goldenAddr

  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/0/0 \
  --- > | cardano-address key public --with-chain-code \
  --- > | cardano-address key hash \
  --- > | cardano-address address payment --network-tag mainnet \
  --- > | cardano-address address delegation $(cat stake.xvk)
  it
    "ANY_ADDRESS_POST_09e - Golden tests for delegating address with mixed credentials" $ \ctx -> do
    let
      payload =
        Json
          [json|{
                "payment": "addr_vkh1gza7wc699kqnjv55ldmj74x0acledxfd7z8zvlvjcwnj2h09mcs",
                "stake": "stake_vk16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7qau558d"
            }|]
    r <- request @AnyAddress ctx Link.postAnyAddress Default payload
    expectResponseCode HTTP.status202 r
    let
      goldenAddr =
        "addr1q9qthemrg5kczwfjjnahwt65elhrl95e9hcgufnajtp6wff5r\
        \h7cflza8t3m5wlaj45sg53nvtwpc73mqk90ghv7vv7s64ryn2"
          :: Text
    validateAddr r goldenAddr

  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/0/0 \
  --- > | cardano-address key public --with-chain-code \
  --- > | cardano-address address payment --network-tag mainnet \
  --- > | cardano-address address delegation $(cat stake.vkh)
  it
    "ANY_ADDRESS_POST_09f - Golden tests for delegating address with mixed credentials" $ \ctx -> do
    let
      payload =
        Json
          [json|{
                "payment": "addr_xvk1lqglg77z6kajsdz4739q22c0zm0yhuy567z6xk2vc0z5ucjtkwp3k2zz8796vdstcu7q0qp232wyvzjes0qkpmt7gzwa0x2q75h3qcgl5y4q0",
                "stake": "stake_vkh1xswlmp8ut5aw8w3mlk2kjpzjxd3dc8r68vzc4azane3n6r07ddx"
            }|]
    r <- request @AnyAddress ctx Link.postAnyAddress Default payload
    expectResponseCode HTTP.status202 r
    let
      goldenAddr =
        "addr1q9qthemrg5kczwfjjnahwt65elhrl95e9hcgufnajtp6wff5r\
        \h7cflza8t3m5wlaj45sg53nvtwpc73mqk90ghv7vv7s64ryn2"
          :: Text
    validateAddr r goldenAddr

  -- Generating golden test data for delegating address - payment from script, stake from pub key:
  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/2/0 \
  --- > | cardano-address key public --with-chain-code > stake.xpub

  --- $ cardano-address script hash "$(cat script.txt)" \
  --- | cardano-address address payment --from-script --network-tag mainnet \
  --- | cardano-address address delegation --from-key $(cat stake.xpub)
  it
    "ANY_ADDRESS_POST_10 - Golden tests for delegating address - payment from script, stake from key" $ \ctx -> do
    let
      payload =
        Json
          [json|{
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
    let
      goldenAddr =
        "addr1zyqmnmwuh85e0fxaggl6ac2hfeqncg76gsr0ld8qdjd84af5rh7cflza8t3m5wlaj45sg53nvtwpc73mqk90ghv7vv7srr0dle"
          :: Text
    validateAddr r goldenAddr

  -- Generating golden test data for delegating address - payment from pub key, stake from script:
  --- $ cat recovery-phrase.txt | cardano-address key from-recovery-phrase Shelley \
  --- > | cardano-address key child 1852H/1815H/0H/0/0 \
  --- > | cardano-address key public --with-chain-code \
  --- > | cardano-address address payment --from-key --network-tag mainnet \
  --- > | cardano-address address delegation --from-script $(cardano-address script hash "$(cat script3.txt)")
  it
    "ANY_ADDRESS_POST_11 - Golden tests for delegating address - payment from key, stake from script" $ \ctx -> do
    let
      payload =
        Json
          [json|{
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
    let
      goldenAddr =
        "addr1y9qthemrg5kczwfjjnahwt65elhrl95e9hcgufnajtp6wfgph8kaew0fj7jd6s3l4ms4wnjp8s3a53qxl76wqmy60t6ssqcamq"
          :: Text
    validateAddr r goldenAddr

  it "ANY_ADDRESS_POST_12 - Delegating addresses API roundtrip" $ \ctx -> runResourceT $ do
    w <- emptyWallet ctx

    -- Generate first 20 addresses using payment and stake keys derived from
    -- wallet API
    let
      indices = [0 .. 19]
    generatedAddresses <- forM indices $ \index -> do
      let
        paymentPath = Link.getWalletKey @'Shelley w UtxoExternal (DerivationIndex index) Nothing
      (_, paymentKey) <-
        unsafeRequest @ApiVerificationKeyShelley ctx paymentPath Empty

      let
        stakePath = Link.getWalletKey @'Shelley w MutableAccount (DerivationIndex 0) Nothing
      (_, stakeKey) <- unsafeRequest @ApiVerificationKeyShelley ctx stakePath Empty

      let
        payload =
          Json
            [json|{
                    "payment": #{paymentKey},
                    "stake": #{stakeKey}
                }|]
      (_, addr) <- unsafeRequest @AnyAddress ctx Link.postAnyAddress payload
      pure (addr ^. #payload)

    -- Make sure the same addresses are already available in the wallet
    addrs <- listAddresses @n ctx w
    forM_ (zip (fmap fromIntegral indices) generatedAddresses)
      $ \(idx, genAddr) -> do
        let
          walAddr = addrs !! idx ^. #id . (#apiAddress . #unAddress)
        walAddr `shouldBe` genAddr

  it "ANY_ADDRESS_POST_13 - Golden tests for script with timelocks" $ \ctx -> do
    let
      payload =
        Json
          [json|{
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
    let
      goldenAddr =
        "addr1xy756z909yycvf5ah8j5pc4cvuedkhvhyylmgfz400t8jdwmwa0hp024gu7dm6h8n252lkgnzemp93mm9kyd48p64mjshqtu3c"
          :: Text
    validateAddr r goldenAddr

  it "ANY_ADDRESS_POST_14a - at_least 0 is valid when non-validated" $ \ctx -> do
    let
      payload =
        Json
          [json|{
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
    let
      payload =
        Json
          [json|{
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

  it
    "ANY_ADDRESS_POST_14c - at_least 0 is not valid when validation is recommended" $ \ctx -> do
    let
      payload =
        Json
          [json|{
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
    let
      payload =
        Json
          [json|{
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
    let
      payload =
        Json
          [json|{
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

  it
    "ANY_ADDRESS_POST_15c - at_least 4 is not valid when validation is recommended" $ \ctx -> do
    let
      payload =
        Json
          [json|{
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

  it
    "ANY_ADDRESS_POST_16a - script with duplicated verification keys is valid when non-validated" $ \ctx -> do
    let
      payload =
        Json
          [json|{
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

  it
    "ANY_ADDRESS_POST_16b - script with duplicated verification keys is valid when required validation used" $ \ctx -> do
    let
      payload =
        Json
          [json|{
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

  it
    "ANY_ADDRESS_POST_16c - script with duplicated verification keys is invalid when recommended validation used" $ \ctx -> do
    let
      payload =
        Json
          [json|{
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

  it
    "ANY_ADDRESS_POST_17a - Script with contradictory timelocks is valid when validation not used" $ \ctx -> do
    let
      payload =
        Json
          [json|{
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

  it
    "ANY_ADDRESS_POST_17b - Script with contradictory timelocks is invalid when required validation is used" $ \ctx -> do
    let
      payload =
        Json
          [json|{
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

  it
    "ANY_ADDRESS_POST_17c - Script with contradictory timelocks is invalid when recommended validation is used" $ \ctx -> do
    let
      payload =
        Json
          [json|{
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

  it
    "ANY_ADDRESS_POST_17d - script with mixed payment/delegation verification keys is invalid" $ \ctx -> do
    let
      payload =
        Json
          [json|{
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
    let
      initPoolGap = 10
    w <- emptyWalletWith ctx ("Wallet", fixturePassphrase, initPoolGap)

    let
      endpoint = Link.postAccountKey @'Shelley w (DerivationIndex 0)
    let
      payload =
        Json
          [json|{
                "passphrase": #{fixturePassphrase},
                "format": "extended"
            }|]
    resp <- request @ApiAccountKey ctx endpoint Default payload
    expectErrorMessage errMsg403WrongIndex resp

    -- Request first 10 extended account public keys
    let
      indices = [0 .. 9]
    accountPublicKeys <- forM indices $ \index -> do
      let
        accountPath = Link.postAccountKey @'Shelley w (DerivationIndex $ 2_147_483_648 + index)
      let
        payload1 =
          Json
            [json|{
                    "passphrase": #{fixturePassphrase},
                    "format": "extended"
                }|]
      let
        payload2 =
          Json
            [json|{
                    "passphrase": #{fixturePassphrase},
                    "format": "non_extended"
                }|]
      (_, accXPub) <- unsafeRequest @ApiAccountKey ctx accountPath payload1
      (_, accPub) <- unsafeRequest @ApiAccountKey ctx accountPath payload2
      let
        (Aeson.String accXPubTxt) = toJSON accXPub
      let
        (Aeson.String accPubTxt) = toJSON accPub
      T.isPrefixOf "acct_xvk" accXPubTxt `shouldBe` True
      T.isPrefixOf "acct_vk" accPubTxt `shouldBe` True
      pure [accXPub, accPub]
    length (concat accountPublicKeys) `shouldBe` 20

  it "POST_ACCOUNT_02 - Can get account public key using purpose" $ \ctx -> runResourceT $ do
    let
      initPoolGap = 10
    w <- emptyWalletWith ctx ("Wallet", fixturePassphrase, initPoolGap)
    let
      accountPath = Link.postAccountKey @'Shelley w (DerivationIndex $ 2_147_483_648 + 1)
    let
      payload1 =
        Json
          [json|{
                "passphrase": #{fixturePassphrase},
                "format": "extended"
            }|]
    (_, accXPub1) <- unsafeRequest @ApiAccountKey ctx accountPath payload1
    let
      (Aeson.String accXPub1Txt) = toJSON accXPub1
    T.isPrefixOf "acct_xvk" accXPub1Txt `shouldBe` True

    let
      payload2 =
        Json
          [json|{
                "passphrase": #{fixturePassphrase},
                "format": "extended",
                "purpose": "1852H"
            }|]
    (_, accXPub2) <- unsafeRequest @ApiAccountKey ctx accountPath payload2
    accXPub1 `shouldBe` accXPub2

    let
      payload3 =
        Json
          [json|{
                "passphrase": #{fixturePassphrase},
                "format": "extended",
                "purpose": "1854H"
            }|]
    (_, accXPub3) <- unsafeRequest @ApiAccountKey ctx accountPath payload3
    accXPub1 `shouldNotBe` accXPub3
    let
      (Aeson.String accXPub3Txt) = toJSON accXPub3
    T.isPrefixOf "acct_shared_xvk" accXPub3Txt `shouldBe` True

    let
      payload4 =
        Json
          [json|{
                "passphrase": #{fixturePassphrase},
                "format": "extended",
                "purpose": "1854"
            }|]
    resp <- request @ApiAccountKey ctx accountPath Default payload4
    expectErrorMessage errMsg403WrongIndex resp

  it "ANY_ADDRESS_POST_15 - Staking address using stake credential non-hashed" $ \ctx -> runResourceT $ do
    w <- emptyWallet ctx

    let
      stakePath =
        Link.getWalletKey @'Shelley w MutableAccount (DerivationIndex 0) Nothing
    (_, stakeKey) <-
      unsafeRequest @ApiVerificationKeyShelley ctx stakePath Empty
    let
      (Aeson.String stakeKeyTxt) = toJSON stakeKey
    stakeKeyTxt `shouldSatisfy` T.isPrefixOf "stake_vk1"

    let
      payload =
        Json
          [json|{
                "stake": #{stakeKey}
            }|]
    (_, stakeAddr) <-
      unsafeRequest @AnyAddress ctx Link.postAnyAddress payload
    let
      (Aeson.Object stakeAddrJson) = toJSON stakeAddr
    let
      (Just (Aeson.String stakeAddrTxt)) =
        KeyMap.lookup "address" stakeAddrJson
    stakeAddrTxt `shouldSatisfy` T.isPrefixOf "stake1"

  it "ANY_ADDRESS_POST_16 - Staking address using stake credential hashed" $ \ctx -> runResourceT $ do
    w <- emptyWallet ctx

    let
      stakePath =
        Link.getWalletKey @'Shelley w MutableAccount (DerivationIndex 0) (Just True)
    (_, stakeKeyHash) <-
      unsafeRequest @ApiVerificationKeyShelley ctx stakePath Empty
    let
      (Aeson.String stakeKeyHashTxt) = toJSON stakeKeyHash
    stakeKeyHashTxt `shouldSatisfy` T.isPrefixOf "stake_vkh1"

    let
      payload =
        Json
          [json|{
                "stake": #{stakeKeyHash}
            }|]
    (_, stakeAddr) <-
      unsafeRequest @AnyAddress ctx Link.postAnyAddress payload
    let
      (Aeson.Object stakeAddrJson) = toJSON stakeAddr
    let
      (Just (Aeson.String stakeAddrTxt)) =
        KeyMap.lookup "address" stakeAddrJson
    stakeAddrTxt `shouldSatisfy` T.isPrefixOf "stake1"
  where
    validateAddr resp expected = do
      let
        addr = getFromResponse id resp
      toJSON addr `shouldBe` object ["address" .= expected]
