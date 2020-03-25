{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Byron.Scenario.API.Transactions
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiByronWallet
    , ApiFee
    , ApiT (..)
    , ApiTransaction
    , DecodeAddress (..)
    , EncodeAddress (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( FromMnemonic (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PaymentAddress (..)
    , hex
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..), generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.Mnemonic
    ( entropyToMnemonic, genEntropy, mnemonicToText )
import Cardano.Wallet.Primitive.Types
    ( Address, Direction (..), TxStatus (..) )
import Control.Monad
    ( forM, forM_ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe, it, shouldBe )
import Test.Integration.Faucet
    ( nextWallet )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , TxDescription (..)
    , between
    , emptyByronWalletFromXPrvWith
    , emptyIcarusWallet
    , emptyRandomWallet
    , eventually
    , expectErrorMessage
    , expectField
    , expectResponseCode
    , faucetAmt
    , fixtureIcarusWallet
    , fixtureIcarusWalletAddrs
    , fixtureIcarusWalletWith
    , fixturePassphrase
    , fixturePassphraseEncrypted
    , fixtureRandomWallet
    , fixtureRandomWalletAddrs
    , fixtureRandomWalletMws
    , fixtureRandomWalletWith
    , icarusAddresses
    , json
    , randomAddresses
    , request
    , verify
    , walletId
    )
import Test.Integration.Framework.Request
    ( RequestException )
import Test.Integration.Framework.TestData
    ( errMsg403Fee
    , errMsg403InputsDepleted
    , errMsg403NotEnoughMoney_
    , errMsg403UTxO
    , errMsg403WrongPass
    , errMsg403WrongPass
    , errMsg404NoWallet
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Aeson as Aeson
import qualified Data.ByteArray as BA
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types.Status as HTTP

spec
    :: forall (n :: NetworkDiscriminant) t.
        ( PaymentAddress n IcarusKey
        , PaymentAddress n ByronKey
        , EncodeAddress n
        , DecodeAddress n
        )
    => SpecWith (Context t)
spec = do
    describe "BYRON_TXS" $ do
        -- Random → Random
        scenario_TRANS_CREATE_01_02 @n fixtureRandomWallet
            [ fixtureRandomWalletAddrs @n
            ]

        -- Random → [Random, Icarus]
        scenario_TRANS_CREATE_01_02 @n fixtureRandomWallet
            [ fixtureRandomWalletAddrs @n
            , fixtureIcarusWalletAddrs @n
            ]

        -- Icarus → Icarus
        scenario_TRANS_CREATE_01_02 @n fixtureIcarusWallet
            [ fixtureIcarusWalletAddrs @n
            ]

        -- Icarus → [Icarus, Random]
        scenario_TRANS_CREATE_01_02 @n fixtureRandomWallet
            [ fixtureIcarusWalletAddrs @n
            , fixtureRandomWalletAddrs @n
            ]

        scenario_TRANS_CREATE_02x @n

        -- TRANS_CREATE_03 requires actually being able to compute exact fees, which
        -- is not really possible w/ cardano-node. So, skipping.

        scenario_TRANS_CREATE_04a @n
        scenario_TRANS_CREATE_04b @n
        scenario_TRANS_CREATE_04c @n
        scenario_TRANS_CREATE_04d @n

        scenario_TRANS_CREATE_07 @n

        scenario_TRANS_ESTIMATE_01_02 @n fixtureRandomWallet
            [ randomAddresses @n . entropyToMnemonic <$> genEntropy
            ]

        scenario_TRANS_ESTIMATE_01_02 @n fixtureIcarusWallet
            [ icarusAddresses @n . entropyToMnemonic <$> genEntropy
            , icarusAddresses @n . entropyToMnemonic <$> genEntropy
            ]

        scenario_TRANS_ESTIMATE_04a @n
        scenario_TRANS_ESTIMATE_04b @n
        scenario_TRANS_ESTIMATE_04c @n

    describe "BYRON_RESTORATION" $ do
        scenario_RESTORE_01 @n fixtureRandomWallet
        scenario_RESTORE_02 @n (fixtureRandomWalletAddrs @n)
        scenario_RESTORE_03 @n (fixtureRandomWalletAddrs @n)
--
-- Scenarios
--

scenario_TRANS_CREATE_01_02
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        )
    => (Context t -> IO ApiByronWallet)
    -> [Context t -> IO (ApiByronWallet, [Address])]
    -> SpecWith (Context t)
scenario_TRANS_CREATE_01_02 fixtureSource fixtures = it title $ \ctx -> do
    -- SETUP
    let amnt = 100_000 :: Natural
    wSrc <- fixtureSource ctx
    (recipients, payments) <- fmap unzip $ forM fixtures $ \fixtureTarget -> do
        (wDest, addrs) <- fixtureTarget ctx
        pure (wDest, mkPayment @n (head addrs) amnt)

    -- ACTION
    r <- postByronTransaction @n ctx wSrc payments fixturePassphrase

    -- ASSERTIONS
    let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
            { nInputs  = fromIntegral n
            , nOutputs = fromIntegral n
            , nChanges = fromIntegral n
            }
    verify r
        [ expectResponseCode HTTP.status202
        , expectField #amount $ between
              ( Quantity (feeMin + n * amnt)
              , Quantity (feeMax + n * amnt)
              )
        , expectField #direction (`shouldBe` ApiT Outgoing)
        , expectField #status (`shouldBe` ApiT Pending)
        ]

    eventually "source balance decreases" $ do
        rSrc <- request @ApiByronWallet ctx
            (Link.getWallet @'Byron wSrc) Default Empty
        verify rSrc
            [ expectField (#balance . #available) $ between
                ( Quantity (faucetAmt - (n * amnt) - feeMax)
                , Quantity (faucetAmt - (n * amnt) - feeMin)
                )
            ]

    forM_ recipients $ \wDest -> do
        eventually "destination balance increases" $ do
            rDest <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron wDest) Default Empty
            verify rDest
                [ expectField (#balance . #available)
                    (`shouldBe` Quantity (faucetAmt + amnt))
                ]
  where
    title = "TRANS_CREATE_01/02 - " ++ show n ++ " recipient(s)"
    n = fromIntegral $ length fixtures

scenario_TRANS_ESTIMATE_01_02
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        )
    => (Context t -> IO ApiByronWallet)
    -> [IO [Address]]
    -> SpecWith (Context t)
scenario_TRANS_ESTIMATE_01_02 fixtureSource fixtures = it title $ \ctx -> do
    -- SETUP
    let amnt = 100_000 :: Natural
    wSrc <- fixtureSource ctx
    payments <- forM fixtures $ \fixtureTarget -> do
        addrs <- fixtureTarget
        pure $ mkPayment @n (head addrs) amnt

    -- ACTION
    r <- estimateByronTransaction ctx wSrc payments

    -- ASSERTIONS
    let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
            { nInputs  = length fixtures
            , nOutputs = length fixtures
            , nChanges = length fixtures
            }
    verify r
        [ expectResponseCode HTTP.status202
        , expectField #amount $ between (Quantity feeMin, Quantity feeMax)
        ]
  where
    title = "TRANS_ESTIMATE_01/02 - " ++ show (length fixtures) ++ " recipient(s)"

scenario_TRANS_CREATE_02x
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n ByronKey
        )
    => SpecWith (Context t)
scenario_TRANS_CREATE_02x = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureSingleUTxO @n ctx

    -- ACTION
    r <- postByronTransaction @n ctx wSrc payments fixturePassphrase

    -- ASSERTIONS
    verify r
        [ expectResponseCode HTTP.status403
        , expectErrorMessage errMsg403UTxO
        ]
  where
    title = "TRANS_CREATE_02x - Multi-output failure w/ single UTxO"

scenario_TRANS_CREATE_04a
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n ByronKey
        )
    => SpecWith (Context t)
scenario_TRANS_CREATE_04a = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureErrInputsDepleted @n ctx

    -- ACTION
    r <- postByronTransaction @n ctx wSrc payments fixturePassphrase

    -- ASSERTIONS
    verify r
        [ expectResponseCode HTTP.status403
        , expectErrorMessage errMsg403InputsDepleted
        ]
  where
    title = "TRANS_CREATE_04 - Error shown when ErrInputsDepleted encountered"

scenario_TRANS_CREATE_04b
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        )
    => SpecWith (Context t)
scenario_TRANS_CREATE_04b = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureCantCoverFee @n ctx

    -- ACTION
    r <- postByronTransaction @n ctx wSrc payments fixturePassphrase

    -- ASSERTIONS
    verify r
        [ expectResponseCode HTTP.status403
        , expectErrorMessage errMsg403Fee
        ]
  where
    title = "TRANS_CREATE_04 - Can't cover fee"

scenario_TRANS_CREATE_04c
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        )
    => SpecWith (Context t)
scenario_TRANS_CREATE_04c = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureNotEnoughMoney @n ctx

    -- ACTION
    r <- postByronTransaction @n ctx wSrc payments fixturePassphrase

    -- ASSERTIONS
    verify r
        [ expectResponseCode HTTP.status403
        , expectErrorMessage errMsg403NotEnoughMoney_
        ]
  where
    title = "TRANS_CREATE_04 - Not enough money"

scenario_TRANS_CREATE_04d
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        )
    => SpecWith (Context t)
scenario_TRANS_CREATE_04d = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureWrongPassphrase @n ctx

    -- ACTION
    r <- postByronTransaction @n ctx wSrc payments "This passphrase is wrong"

    -- ASSERTIONS
    verify r
        [ expectResponseCode HTTP.status403
        , expectErrorMessage errMsg403WrongPass
        ]
  where
    title = "TRANS_CREATE_04 - Wrong password"

scenario_TRANS_ESTIMATE_04a
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n ByronKey
        )
    => SpecWith (Context t)
scenario_TRANS_ESTIMATE_04a = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureErrInputsDepleted @n ctx

    -- ACTION
    r <- estimateByronTransaction ctx wSrc payments

    -- ASSERTIONS
    verify r
        [ expectResponseCode HTTP.status403
        , expectErrorMessage errMsg403InputsDepleted
        ]
  where
    title = "TRANS_ESTIMATE_04 - Error shown when ErrInputsDepleted encountered"

scenario_TRANS_ESTIMATE_04b
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        )
    => SpecWith (Context t)
scenario_TRANS_ESTIMATE_04b = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureCantCoverFee @n ctx

    -- ACTION
    r <- estimateByronTransaction ctx wSrc payments

    -- ASSERTIONS
    let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
            { nInputs  = 1
            , nOutputs = 1
            , nChanges = 0
            }
    verify r
        [ expectResponseCode HTTP.status202
        , expectField #amount $ between (Quantity feeMin, Quantity feeMax)
        ]
  where
    title = "TRANS_ESTIMATE_04 - Can't cover fee"

scenario_TRANS_ESTIMATE_04c
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        )
    => SpecWith (Context t)
scenario_TRANS_ESTIMATE_04c = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureNotEnoughMoney @n ctx

    -- ACTION
    r <- estimateByronTransaction ctx wSrc payments

    -- ASSERTIONS
    verify r
        [ expectResponseCode HTTP.status403
        , expectErrorMessage errMsg403NotEnoughMoney_
        ]
  where
    title = "TRANS_ESTIMATE_04 - Not enough money"

scenario_TRANS_CREATE_07
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n ByronKey
        )
    => SpecWith (Context t)
scenario_TRANS_CREATE_07 = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureDeletedWallet @n ctx

    -- ACTION
    r <- postByronTransaction @n ctx wSrc payments fixturePassphrase

    -- ASSERTIONS
    verify r
        [ expectResponseCode @IO HTTP.status404
        , expectErrorMessage $ errMsg404NoWallet (wSrc ^. walletId)
        ]
  where
    title = "TRANS_CREATE_07 - Deleted wallet"

scenario_RESTORE_01
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n ByronKey
        )
    => (Context t -> IO ApiByronWallet)
    -> SpecWith (Context t)
scenario_RESTORE_01 fixtureSource = it title $ \ctx -> do
    -- SETUP
    let amnt = 100_000 :: Natural
    wSrc <- fixtureSource ctx
    (wDest, payment, mnemonics) <- do
        (wDest, mnemonics) <- fixtureRandomWalletMws ctx
        let addrs = randomAddresses @n mnemonics
        pure (wDest, mkPayment @n (head addrs) amnt, mnemonics)

    -- ACTION
    r <- postByronTransaction @n ctx wSrc [payment] fixturePassphrase

    -- ASSERTIONS
    let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
            { nInputs  = 1
            , nOutputs = 1
            , nChanges = 1
            }
    verify r
        [ expectResponseCode HTTP.status202
        , expectField #amount $ between
              ( Quantity (feeMin + amnt)
              , Quantity (feeMax + amnt)
              )
        , expectField #direction (`shouldBe` ApiT Outgoing)
        , expectField #status (`shouldBe` ApiT Pending)
        ]

    eventually "source balance decreases" $ do
        rSrc <- request @ApiByronWallet ctx
            (Link.getWallet @'Byron wSrc) Default Empty
        verify rSrc
            [ expectField (#balance . #available) $ between
                ( Quantity (faucetAmt - amnt - feeMax)
                , Quantity (faucetAmt - amnt - feeMin)
                )
            ]

    eventually "destination balance increases" $ do
        rDest <- request @ApiByronWallet ctx
            (Link.getWallet @'Byron wDest) Default Empty
        verify rDest
            [ expectField (#balance . #available)
                (`shouldBe` Quantity (faucetAmt + amnt))
            ]

    -- ACTION
    rd1 <- request
           @ApiByronWallet ctx (Link.deleteWallet @'Byron wDest) Default Empty
    expectResponseCode @IO HTTP.status204 rd1

    -- MORE SETUP
    let (Right seed) = fromMnemonic @'[12] (mnemonicToText mnemonics)
    let rootXPrv = T.decodeUtf8 $ hex $ getKey $
            generateKeyFromSeed seed
            (Passphrase $ BA.convert $ T.encodeUtf8 fixturePassphrase)

    -- ACTION
    wDestRestored <- emptyByronWalletFromXPrvWith ctx "random"
            ("Byron Wallet Restored", rootXPrv, fixturePassphraseEncrypted)

    -- ASSERTIONS
    eventually "destination balance increases" $ do
        rDest <- request @ApiByronWallet ctx
            (Link.getWallet @'Byron wDestRestored) Default Empty
        verify rDest
            [ expectField (#balance . #available)
                (`shouldBe` Quantity (faucetAmt + amnt))
            ]
  where
    title = "BYRON_RESTORE_01 - can restore recipient wallet from xprv"

scenario_RESTORE_02
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        )
    => (Context t -> IO (ApiByronWallet, [Address]))
    -> SpecWith (Context t)
scenario_RESTORE_02 fixtureTarget = it title $ \ctx -> do
    -- SETUP
    let amnt = 100_000 :: Natural
    mnemonics <- mnemonicToText <$> nextWallet @"random" (_faucet ctx)
    let (Right seed) = fromMnemonic @'[12] mnemonics
    let rootXPrv = T.decodeUtf8 $ hex $ getKey $
            generateKeyFromSeed seed
            (Passphrase $ BA.convert $ T.encodeUtf8 fixturePassphrase)
    wSrc <- emptyByronWalletFromXPrvWith ctx "random"
            ("Byron Wallet Restored", rootXPrv, fixturePassphraseEncrypted)
    (wDest, payment) <- do
        (wDest, addrs) <- fixtureTarget ctx
        pure (wDest, mkPayment @n (head addrs) amnt)

    -- ACTION
    r <- postByronTransaction @n ctx wSrc [payment] fixturePassphrase

    -- ASSERTIONS
    let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
            { nInputs  = 1
            , nOutputs = 1
            , nChanges = 1
            }
    verify r
        [ expectResponseCode HTTP.status202
        , expectField #amount $ between
              ( Quantity (feeMin + amnt)
              , Quantity (feeMax + amnt)
              )
        , expectField #direction (`shouldBe` ApiT Outgoing)
        , expectField #status (`shouldBe` ApiT Pending)
        ]

    eventually "source balance decreases" $ do
        rSrc <- request @ApiByronWallet ctx
            (Link.getWallet @'Byron wSrc) Default Empty
        verify rSrc
            [ expectField (#balance . #available) $ between
                ( Quantity (faucetAmt - amnt - feeMax)
                , Quantity (faucetAmt - amnt - feeMin)
                )
            ]

    eventually "destination balance increases" $ do
        rDest <- request @ApiByronWallet ctx
            (Link.getWallet @'Byron wDest) Default Empty
        verify rDest
            [ expectField (#balance . #available)
                (`shouldBe` Quantity (faucetAmt + amnt))
            ]
  where
    title = "BYRON_RESTORE_02 - can send tx from restored wallet from xprv"

scenario_RESTORE_03
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        )
    => (Context t -> IO (ApiByronWallet, [Address]))
    -> SpecWith (Context t)
scenario_RESTORE_03 fixtureTarget = it title $ \ctx -> do
    -- SETUP
    mnemonics <- mnemonicToText <$> nextWallet @"random" (_faucet ctx)
    let (Right seed) = fromMnemonic @'[12] mnemonics
    let rootXPrv = T.decodeUtf8 $ hex $ getKey $
            generateKeyFromSeed seed
            (Passphrase $ BA.convert $ T.encodeUtf8 fixturePassphrase)
    let passHashCorrupted = T.replicate 100 "0"
    let amnt = 100_000 :: Natural
    (_, payment) <- do
        (wDest, addrs) <- fixtureTarget ctx
        pure (wDest, mkPayment @n (head addrs) amnt)

    -- ACTION
    wSrc <- emptyByronWalletFromXPrvWith ctx "random"
            ("Byron Wallet Restored", rootXPrv, passHashCorrupted)
    rSrc <- request @ApiByronWallet ctx
            (Link.getWallet @'Byron wSrc) Default Empty
    -- ASSERTIONS
    verify rSrc
        [ expectField (#balance . #available)
          (`shouldBe` Quantity faucetAmt)
        ]

    -- ACTION
    r <- postByronTransaction @n ctx wSrc [payment] fixturePassphrase
    -- ASSERTIONS
    verify r
        [ expectResponseCode HTTP.status403
        , expectErrorMessage errMsg403WrongPass
        ]

  where
    title = "BYRON_RESTORE_03 - restoring wallet from corrupted hash gives\
            \ proper balance but sending tx fails"


--
-- More Elaborated Fixtures
--

-- | Returns a source wallet and a list of payments.
--
-- NOTE: Random or Icarus wallets can be used interchangeably here.
fixtureSingleUTxO
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n ByronKey
        )
    => Context t
    -> IO (ApiByronWallet, [Aeson.Value])
fixtureSingleUTxO ctx = do
    wSrc  <- fixtureRandomWalletWith @n ctx [1_000_000]
    addrs <- randomAddresses @n . entropyToMnemonic <$> genEntropy
    let payments =
            [ mkPayment @n (head addrs) 100_000
            , mkPayment @n (head addrs) 100_000
            ]
    pure (wSrc, payments)

-- | Returns a source wallet and a list of payments. If submitted, the payments
-- should result in an error 403.
--
-- NOTE: Random or Icarus wallets can be used interchangeably here.
fixtureErrInputsDepleted
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n ByronKey
        )
    => Context t
    -> IO (ApiByronWallet, [Aeson.Value])
fixtureErrInputsDepleted ctx = do
    wSrc  <- fixtureRandomWalletWith @n ctx [12_000_000, 20_000_000, 17_000_000]
    addrs <- randomAddresses @n . entropyToMnemonic <$> genEntropy
    let amnts = [40_000_000, 22, 22] :: [Natural]
    let payments = flip map (zip addrs amnts) $ uncurry (mkPayment @n)
    pure (wSrc, payments)

-- | Returns a source wallet and a list of payments.
--
-- NOTE: Random or Icarus wallets can be used interchangeably here.
fixtureCantCoverFee
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        )
    => Context t
    -> IO (ApiByronWallet, [Aeson.Value])
fixtureCantCoverFee ctx = do
    let (feeMin, _) = ctx ^. #_feeEstimator $ PaymentDescription 1 1 1
    wSrc <- fixtureIcarusWalletWith @n ctx [feeMin `div` 2]
    addrs <- icarusAddresses @n . entropyToMnemonic <$> genEntropy
    pure (wSrc, [mkPayment @n (head addrs) 1])

-- | Returns a source wallet and a list of payments.
--
-- NOTE: Random or Icarus wallets can be used interchangeably here.
fixtureNotEnoughMoney
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        )
    => Context t
    -> IO (ApiByronWallet, [Aeson.Value])
fixtureNotEnoughMoney ctx = do
    wSrc <- emptyIcarusWallet ctx
    addrs <- icarusAddresses @n . entropyToMnemonic <$> genEntropy
    pure (wSrc, [mkPayment @n (head addrs) 1])

-- | Returns a source wallet and a list of payments.
--
-- NOTE: Random or Icarus wallets can be used interchangeably here.
fixtureWrongPassphrase
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        )
    => Context t
    -> IO (ApiByronWallet, [Aeson.Value])
fixtureWrongPassphrase ctx = do
    (wSrc, addrs) <- fixtureIcarusWalletAddrs @n ctx
    pure (wSrc, [mkPayment @n (head addrs) 100_000])

-- | Returns a source wallet and a list of payments.
--
-- NOTE: Random or Icarus wallets can be used interchangeably here.
fixtureDeletedWallet
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n ByronKey
        )
    => Context t
    -> IO (ApiByronWallet, [Aeson.Value])
fixtureDeletedWallet ctx = do
    wSrc <- emptyRandomWallet ctx
    _ <- request @() ctx (Link.deleteWallet @'Byron wSrc) Default Empty
    addrs <- randomAddresses @n . entropyToMnemonic <$> genEntropy
    pure (wSrc, [mkPayment @n (head addrs) 100_000])

--
-- Helpers
--

-- | Construct a JSON payload for a single payment (address + amount)
mkPayment
    :: forall (n :: NetworkDiscriminant).
        ( EncodeAddress n
        )
    => Address
    -> Natural
    -> Aeson.Value
mkPayment addr_ amnt = [json|
    { "address": #{addr}
    , "amount":
        { "quantity": #{amnt}
        , "unit": "lovelace"
        }
    } |]
  where
    addr = encodeAddress @n addr_

postByronTransaction
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        )
    => Context t
        -- A surrounding API context
    -> ApiByronWallet
        -- ^ Source wallet
    -> [Aeson.Value]
        -- ^ A list of payment
    -> Text
        -- ^ Passphrase
    -> IO (HTTP.Status, Either RequestException (ApiTransaction n))
postByronTransaction ctx wSrc payments passphrase = do
    let body = [json|
            { "payments": #{payments}
            , "passphrase": #{passphrase}
            }|]
    request ctx (Link.createTransaction @'Byron wSrc) Default (Json body)

estimateByronTransaction
    :: Context t
        -- A surrounding API context
    -> ApiByronWallet
        -- ^ Source wallet
    -> [Aeson.Value]
        -- ^ A list of payment
    -> IO (HTTP.Status, Either RequestException ApiFee)
estimateByronTransaction ctx wSrc payments = do
    let body = [json| { "payments": #{payments} }|]
    request ctx (Link.getTransactionFee @'Byron wSrc) Default (Json body)
