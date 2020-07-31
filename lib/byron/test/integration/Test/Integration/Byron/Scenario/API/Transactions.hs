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

import Cardano.Mnemonic
    ( entropyToMnemonic, genEntropy, mnemonicToText )
import Cardano.Wallet.Api.Types
    ( ApiAddress (..)
    , ApiByronWallet
    , ApiFee
    , ApiT (..)
    , ApiTransaction
    , ApiTxId (ApiTxId)
    , ApiUtxoStatistics
<<<<<<< HEAD
    , DecodeAddress (..)
    , DecodeStakeAddress (..)
    , EncodeAddress (..)
=======
    , ApiWalletMigrationInfo
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
    , Iso8601Time (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( Direction (..), TxStatus (..) )
import Control.Monad
    ( forM, forM_ )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Maybe
    ( fromJust, isJust )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe, shouldBe, shouldNotBe, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
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
    , expectListField
    , expectListSize
    , expectResponseCode
    , expectWalletUTxO
    , faucetAmt
    , faucetUtxoAmt
    , fixtureIcarusWallet
    , fixtureIcarusWalletAddrs
    , fixtureIcarusWalletWith
    , fixturePassphrase
    , fixturePassphraseEncrypted
    , fixtureRandomWallet
    , fixtureRandomWalletAddrs
    , fixtureRandomWalletMws
    , getFromResponse
    , icarusAddresses
    , json
    , randomAddresses
    , request
    , rootPrvKeyFromMnemonics
    , verify
    , walletId
    )
import Test.Integration.Framework.Request
    ( RequestException )
import Test.Integration.Framework.TestData
    ( errMsg403Fee
    , errMsg403NotEnoughMoney_
    , errMsg403WrongPass
    , errMsg404NoWallet
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

-- TODO: Taking NetworkDiscriminant as an argument is superfluous when it already
-- exists within @Context@.
spec
<<<<<<< HEAD
    :: forall (n :: NetworkDiscriminant) t.
        ( PaymentAddress n IcarusKey
        , PaymentAddress n ByronKey
        , EncodeAddress n
        , DecodeAddress n
        , DecodeStakeAddress n
        )
    => SpecWith (Context t)
spec = do
=======
    :: forall t. NetworkDiscriminant -> SpecWith (Context t)
spec n = do
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
    describe "BYRON_TXS" $ do
        -- Random → Random
        scenario_TRANS_CREATE_01_02 fixtureRandomWallet
            [ fixtureRandomWalletAddrs n
            ]

        -- Random → [Random, Icarus]
        scenario_TRANS_CREATE_01_02 fixtureRandomWallet
            [ fixtureRandomWalletAddrs n
            , fixtureIcarusWalletAddrs n
            ]

        -- Icarus → Icarus
        scenario_TRANS_CREATE_01_02 fixtureIcarusWallet
            [ fixtureIcarusWalletAddrs n
            ]

        -- Icarus → [Icarus, Random]
        scenario_TRANS_CREATE_01_02 fixtureRandomWallet
            [ fixtureIcarusWalletAddrs n
            , fixtureRandomWalletAddrs n
            ]

<<<<<<< HEAD
        -- TRANS_CREATE_03 requires actually being able to compute exact fees, which
        -- is not really possible w/ cardano-node. So, skipping.

        scenario_TRANS_CREATE_04b @n
        scenario_TRANS_CREATE_04c @n
        scenario_TRANS_CREATE_04d @n
=======
        scenario_TRANS_CREATE_02x

        -- TRANS_CREATE_03 requires actually being able to compute exact fees, which
        -- is not really possible w/ cardano-node. So, skipping.

        scenario_TRANS_CREATE_04a
        scenario_TRANS_CREATE_04b
        scenario_TRANS_CREATE_04c
        scenario_TRANS_CREATE_04d
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant

        scenario_TRANS_CREATE_07

        scenario_TRANS_ESTIMATE_01_02 fixtureRandomWallet
            [ randomAddresses n . entropyToMnemonic <$> genEntropy
            ]

        scenario_TRANS_ESTIMATE_01_02 fixtureIcarusWallet
            [ icarusAddresses n . entropyToMnemonic <$> genEntropy
            , icarusAddresses n . entropyToMnemonic <$> genEntropy
            ]

<<<<<<< HEAD
        scenario_TRANS_ESTIMATE_04b @n
        scenario_TRANS_ESTIMATE_04c @n
=======
        scenario_TRANS_ESTIMATE_04a
        scenario_TRANS_ESTIMATE_04b
        scenario_TRANS_ESTIMATE_04c
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant

        scenario_TRANS_REG_1670 (fixtureIcarusWalletWith)

    describe "BYRON_RESTORATION" $ do
        scenario_RESTORE_01 fixtureRandomWallet
        scenario_RESTORE_02 (fixtureRandomWalletAddrs n)
        scenario_RESTORE_03 (fixtureRandomWalletAddrs n)

    describe "BYRON_UTXO" $ do
        scenario_TRANS_UTXO_01 fixtureIcarusWallet (fixtureIcarusWalletAddrs n)
        scenario_TRANS_UTXO_01 fixtureRandomWallet (fixtureRandomWalletAddrs n)

<<<<<<< HEAD
=======
    describe "BYRON_MIGRATE" $ do
        scenario_MIGRATE_01 fixtureRandomWallet
        scenario_MIGRATE_02 fixtureRandomWallet 1
        scenario_MIGRATE_02 fixtureRandomWallet 3
        scenario_MIGRATE_02 fixtureRandomWallet 10
        scenario_MIGRATE_02 fixtureIcarusWallet 1
        scenario_MIGRATE_02 fixtureIcarusWallet 3
        scenario_MIGRATE_02 fixtureIcarusWallet 10

>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
--
-- Scenarios
--

scenario_TRANS_CREATE_01_02
<<<<<<< HEAD
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        )
    => (Context t -> IO ApiByronWallet)
    -> [Context t -> IO (ApiByronWallet, [Address])]
=======
    :: (Context t -> IO ApiByronWallet)
    -> [Context t -> IO (ApiByronWallet, [ApiAddress])]
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
    -> SpecWith (Context t)
scenario_TRANS_CREATE_01_02 fixtureSource fixtures = it title $ \ctx -> do
    -- SETUP
    let amnt = 100_000 :: Natural
    wSrc <- fixtureSource ctx
    (recipients, payments) <- fmap unzip $ forM fixtures $ \fixtureTarget -> do
        (wDest, addrs) <- fixtureTarget ctx
        pure (wDest, mkPayment (head addrs) amnt)

    -- ACTION
<<<<<<< HEAD
    r <- postByronTransaction @n ctx wSrc payments fixturePassphrase
    let txid = getFromResponse #id r
=======
    r <- postByronTransaction ctx wSrc payments fixturePassphrase
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant

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
        , expectField #depth (`shouldBe` Nothing)
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
        let link = Link.listTransactions @'Byron wDest
        rTrans <- request @([ApiTransaction]) ctx link Default Empty
        verify rTrans
            [ expectResponseCode @IO HTTP.status200
            , expectListSize 11
            , expectListField 10 (#status . #getApiT) (`shouldBe` InLedger)
            , expectListField 10 #depth (`shouldNotBe` Nothing)
            ]
        -- Verify one can get incoming tx by ID from dst wallet
        let linkInc = Link.getTransaction @'Byron wDest (ApiTxId txid)
        rInc <- request @(ApiTransaction n) ctx linkInc Default Empty
        verify rInc
            [ expectResponseCode HTTP.status200
            , expectField (#direction . #getApiT) (`shouldBe` Incoming)
            , expectField (#status . #getApiT) (`shouldBe` InLedger)
            ]

    -- Verify one can get outgoing tx by ID from src wallet
    let linkOut = Link.getTransaction @'Byron wSrc (ApiTxId txid)
    rOut <- request @(ApiTransaction n) ctx linkOut Default Empty
    verify rOut
        [ expectResponseCode HTTP.status200
        , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
        , expectField (#status . #getApiT) (`shouldBe` InLedger)
        ]

  where
    title = "TRANS_CREATE_01/02, TRANS_GET_01 - " ++ show n ++ " recipient(s)"
    n = fromIntegral $ length fixtures

scenario_TRANS_ESTIMATE_01_02
<<<<<<< HEAD
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        )
    => (Context t -> IO ApiByronWallet)
    -> [IO [Address]]
=======
    :: (Context t -> IO ApiByronWallet)
    -> [IO [ApiAddress]]
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
    -> SpecWith (Context t)
scenario_TRANS_ESTIMATE_01_02 fixtureSource fixtures = it title $ \ctx -> do
    -- SETUP
    let amnt = 100_000 :: Natural
    wSrc <- fixtureSource ctx
    payments <- forM fixtures $ \fixtureTarget -> do
        addrs <- fixtureTarget
        pure $ mkPayment (head addrs) amnt

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
        , expectField #estimatedMin $ between (Quantity feeMin, Quantity feeMax)
        ]
  where
    title = "TRANS_ESTIMATE_01/02 - " ++ show (length fixtures) ++ " recipient(s)"

<<<<<<< HEAD
scenario_TRANS_CREATE_04b
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        )
    => SpecWith (Context t)
=======
scenario_TRANS_CREATE_02x
    :: SpecWith (Context t)
scenario_TRANS_CREATE_02x = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureSingleUTxO ctx

    -- ACTION
    r <- postByronTransaction ctx wSrc payments fixturePassphrase

    -- ASSERTIONS
    verify r
        [ expectResponseCode HTTP.status403
        , expectErrorMessage errMsg403UTxO
        ]
  where
    title = "TRANS_CREATE_02x - Multi-output failure w/ single UTxO"

scenario_TRANS_CREATE_04a
    :: SpecWith (Context t)
scenario_TRANS_CREATE_04a = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureErrInputsDepleted ctx

    -- ACTION
    r <- postByronTransaction ctx wSrc payments fixturePassphrase

    -- ASSERTIONS
    verify r
        [ expectResponseCode HTTP.status403
        , expectErrorMessage errMsg403InputsDepleted
        ]
  where
    title = "TRANS_CREATE_04 - Error shown when ErrInputsDepleted encountered"

scenario_TRANS_CREATE_04b
    :: SpecWith (Context t)
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
scenario_TRANS_CREATE_04b = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureCantCoverFee ctx

    -- ACTION
    r <- postByronTransaction ctx wSrc payments fixturePassphrase

    -- ASSERTIONS
    verify r
        [ expectResponseCode HTTP.status403
        , expectErrorMessage errMsg403Fee
        ]
  where
    title = "TRANS_CREATE_04 - Can't cover fee"

scenario_TRANS_CREATE_04c
<<<<<<< HEAD
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        )
    => SpecWith (Context t)
=======
    :: SpecWith (Context t)
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
scenario_TRANS_CREATE_04c = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureNotEnoughMoney ctx

    -- ACTION
    r <- postByronTransaction ctx wSrc payments fixturePassphrase

    -- ASSERTIONS
    verify r
        [ expectResponseCode HTTP.status403
        , expectErrorMessage errMsg403NotEnoughMoney_
        ]
  where
    title = "TRANS_CREATE_04 - Not enough money"

scenario_TRANS_CREATE_04d
<<<<<<< HEAD
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        )
    => SpecWith (Context t)
=======
    :: SpecWith (Context t)
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
scenario_TRANS_CREATE_04d = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureWrongPassphrase ctx

    -- ACTION
    r <- postByronTransaction ctx wSrc payments "This passphrase is wrong"

    -- ASSERTIONS
    verify r
        [ expectResponseCode HTTP.status403
        , expectErrorMessage errMsg403WrongPass
        ]
  where
    title = "TRANS_CREATE_04 - Wrong password"

<<<<<<< HEAD
scenario_TRANS_ESTIMATE_04b
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        )
    => SpecWith (Context t)
=======
scenario_TRANS_ESTIMATE_04a
    :: SpecWith (Context t)
scenario_TRANS_ESTIMATE_04a = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureErrInputsDepleted ctx

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
    :: SpecWith (Context t)
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
scenario_TRANS_ESTIMATE_04b = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureCantCoverFee ctx

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
        , expectField #estimatedMin $ between (Quantity feeMin, Quantity feeMax)
        ]
  where
    title = "TRANS_ESTIMATE_04 - Can't cover fee"

scenario_TRANS_ESTIMATE_04c
<<<<<<< HEAD
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        )
    => SpecWith (Context t)
=======
    :: SpecWith (Context t)
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
scenario_TRANS_ESTIMATE_04c = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureNotEnoughMoney ctx

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
<<<<<<< HEAD
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        , PaymentAddress n ByronKey
        )
    => SpecWith (Context t)
=======
    :: SpecWith (Context t)
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
scenario_TRANS_CREATE_07 = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureDeletedWallet ctx

    -- ACTION
    r <- postByronTransaction ctx wSrc payments fixturePassphrase

    -- ASSERTIONS
    verify r
        [ expectResponseCode @IO HTTP.status404
        , expectErrorMessage $ errMsg404NoWallet (wSrc ^. walletId)
        ]
  where
    title = "TRANS_CREATE_07 - Deleted wallet"

scenario_RESTORE_01
<<<<<<< HEAD
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        , PaymentAddress n ByronKey
        )
    => (Context t -> IO ApiByronWallet)
=======
    :: (Context t -> IO ApiByronWallet)
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
    -> SpecWith (Context t)
scenario_RESTORE_01 fixtureSource = it title $ \ctx -> do
    -- SETUP
    let amnt = 100_000 :: Natural
    wSrc <- fixtureSource ctx
    (wDest, payment, mnemonics) <- do
        (wDest, mnemonics) <- fixtureRandomWalletMws ctx
        let addrs = randomAddresses (_network ctx) mnemonics
        pure (wDest, mkPayment (head addrs) amnt, mnemonics)

    -- ACTION
    r <- postByronTransaction ctx wSrc [payment] fixturePassphrase

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
    let rootXPrv = rootPrvKeyFromMnemonics (mnemonicToText mnemonics) fixturePassphrase

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
<<<<<<< HEAD
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        )
    => (Context t -> IO (ApiByronWallet, [Address]))
=======
    :: (Context t -> IO (ApiByronWallet, [ApiAddress]))
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
    -> SpecWith (Context t)
scenario_RESTORE_02 fixtureTarget = it title $ \ctx -> do
    -- SETUP
    let amnt = 100_000 :: Natural
    mnemonics <- mnemonicToText <$> nextWallet @"random" (_faucet ctx)
    let rootXPrv = rootPrvKeyFromMnemonics mnemonics fixturePassphrase
    wSrc <- emptyByronWalletFromXPrvWith ctx "random"
            ("Byron Wallet Restored", rootXPrv, fixturePassphraseEncrypted)
    (wDest, payment) <- do
        (wDest, addrs) <- fixtureTarget ctx
        pure (wDest, mkPayment (head addrs) amnt)

    -- ACTION
    r <- postByronTransaction ctx wSrc [payment] fixturePassphrase

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
<<<<<<< HEAD
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        )
    => (Context t -> IO (ApiByronWallet, [Address]))
=======
    :: (Context t -> IO (ApiByronWallet, [ApiAddress]))
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
    -> SpecWith (Context t)
scenario_RESTORE_03 fixtureTarget = it title $ \ctx -> do
    -- SETUP
    mnemonics <- mnemonicToText <$> nextWallet @"random" (_faucet ctx)
    let rootXPrv = rootPrvKeyFromMnemonics mnemonics fixturePassphrase
    let passHashCorrupted = T.replicate 100 "0"
    let amnt = 100_000 :: Natural
    (_, payment) <- do
        (wDest, addrs) <- fixtureTarget ctx
        pure (wDest, mkPayment (head addrs) amnt)

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
    r <- postByronTransaction ctx wSrc [payment] fixturePassphrase
    -- ASSERTIONS
    verify r
        [ expectResponseCode HTTP.status403
        , expectErrorMessage errMsg403WrongPass
        ]

  where
    title = "BYRON_RESTORE_03 - restoring wallet from corrupted hash gives\
            \ proper balance but sending tx fails"

scenario_TRANS_UTXO_01
<<<<<<< HEAD
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        )
    => (Context t -> IO ApiByronWallet)
    -> (Context t -> IO (ApiByronWallet, [Address]))
=======
    :: (Context t -> IO ApiByronWallet)
    -> (Context t -> IO (ApiByronWallet, [ApiAddress]))
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
    -> SpecWith (Context t)
scenario_TRANS_UTXO_01 fixtureSource fixtureTarget = it title $ \ctx -> do
    -- SETUP
    wSrc <- fixtureSource ctx
    let coins = [13::Natural, 43, 66, 101, 1339]
    let matrix = zip coins [1..]
    (wDest, addrs) <- fixtureTarget ctx

    forM_ matrix $ \(c, alreadyAbsorbed) -> do
        let payment = mkPayment (head addrs) c

        -- ACTION
        r <- postByronTransaction ctx wSrc [payment] fixturePassphrase
        let coinsSent = map fromIntegral $ take alreadyAbsorbed coins
        let coinsSentAll = sum coinsSent

        --ASSERTIONS
        verify r
            [ expectResponseCode HTTP.status202
            ]
        eventually "destination balance increases" $ do
            rDest <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron wDest) Default Empty
            verify rDest
                [ expectField (#balance . #available)
                  (`shouldBe` Quantity (faucetAmt + coinsSentAll))
                ]
        rStat <- request @ApiUtxoStatistics ctx
                 (Link.getUTxOsStatistics @'Byron wDest) Default Empty
        verify rStat
            [ expectResponseCode HTTP.status200
            ]
        let expectedUtxos =
                fromIntegral <$> (replicate 10 faucetUtxoAmt ++ coinsSent)
        expectWalletUTxO expectedUtxos (snd rStat)
  where
    title = "TRANS_UTXO_01 - one recipient multiple txs received"

<<<<<<< HEAD
scenario_TRANS_REG_1670
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        )
    => (Context t -> [Natural] -> IO ApiByronWallet)
=======
scenario_MIGRATE_01
    :: (Context t -> IO ApiByronWallet)
    -> SpecWith (Context t)
scenario_MIGRATE_01 fixtureSource = it title $ \ctx -> do
    wSrc <- fixtureSource ctx

    r <- request @[ApiTransaction] ctx
         (Link.migrateWallet @'Byron wSrc)
         Default
         (NonJson "{passphrase:,}")
    expectResponseCode @IO HTTP.status400 r
    expectErrorMessage errMsg400ParseError r
  where
    title = "BYRON_MIGRATE_01 - invalid payload, parser error"

scenario_MIGRATE_02
    :: (Context t -> IO ApiByronWallet)
    -> Int
    -> SpecWith (Context t)
scenario_MIGRATE_02 fixtureSource addrCount = it title $ \ctx -> do

    let n = _network ctx

    -- Restore a Byron wallet with funds, to act as a source wallet:
    wSrc <- fixtureSource ctx
    let originalBalance =
            view (#balance . #available . #getQuantity) wSrc

    -- Create an empty target wallet:
    (wDest, mw) <- emptyRandomWalletMws ctx
    let addresses :: [Text] =
            take addrCount $ apiAddress <$> randomAddresses n mw

    -- Calculate the expected migration fee:
    r0 <- request @ApiWalletMigrationInfo ctx
          (Link.getMigrationInfo @'Byron wSrc) Default Empty
    verify r0
        [ expectResponseCode @IO HTTP.status200
        , expectField #migrationCost (.> Quantity 0)
        ]
    let expectedFee = getFromResponse (#migrationCost . #getQuantity) r0

    -- Perform a migration from the source wallet to the target wallet:
    r1 <- request @[ApiTransaction] ctx
          (Link.migrateWallet @'Byron wSrc)
          Default
          (Json [json|
              { passphrase: #{fixturePassphrase}
              , addresses: #{addresses}
              }|])
    verify r1
        [ expectResponseCode @IO HTTP.status202
        , expectField id (`shouldSatisfy` (not . null))
        ]

    -- Check that funds become available in the target wallet:
    let expectedBalance = originalBalance - expectedFee
    eventually "Wallet has expectedBalance" $ do
        r2 <- request @ApiByronWallet ctx
              (Link.getWallet @'Byron wDest) Default Empty
        verify r2
            [ expectField
                (#balance . #available)
                (`shouldBe` Quantity expectedBalance)
            , expectField
                (#balance . #total)
                (`shouldBe` Quantity expectedBalance)
            ]
  where
    title = "BYRON_MIGRATE_02 - after a migration operation successfully \
            \completes, the correct amount eventually becomes available \
            \in the target wallet for an arbitrary number of specified addresses."

scenario_TRANS_REG_1670
    :: (Context t -> [Natural] -> IO ApiByronWallet)
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
    -> SpecWith (Context t)
scenario_TRANS_REG_1670 fixture = it title $ \ctx -> do
    let n = _network ctx

    -- SETUP
    -- We want to construct two transactions, where the second transaction uses
    -- an output of the first one. We achieve this by having a wallet with a
    -- single UTxO, the second transaction is guaranteed to re-use the change
    -- output of the first transaction.
    --                    __
    --                   /   Output           __ Output
    --  Initial UTxO ---x                    /
    --                   \__ Change --------x
    --                                       \__ Change
    --                <-------------->  <--------------->
    --                     1st TX             2nd TX

    let amnt = 1
    let (_, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription 1 1 1
    wSrc <- fixture ctx [2*amnt+2*feeMax]
    [sink] <- take 1 . icarusAddresses n . entropyToMnemonic <$> genEntropy
    let payment = mkPayment sink amnt

    -- 1st TX
    _ <- postByronTransaction ctx wSrc [payment] fixturePassphrase
    eventually "transaction is inserted" $ do
        rTxs <- request @[ApiTransaction] ctx
            (Link.listTransactions @'Byron wSrc) Default Empty
        verify rTxs
            [ expectListField 0 (#status . #getApiT) (`shouldBe` InLedger)
            , expectListField 1 (#status . #getApiT) (`shouldBe` InLedger)
            ]

    -- 2nd TX
    _ <- postByronTransaction ctx wSrc [payment] fixturePassphrase
    start <- eventually "transaction is inserted" $ do
        rTxs <- request @[ApiTransaction] ctx
            (Link.listTransactions @'Byron wSrc) Default Empty
        verify rTxs
            [ expectListField 0 (#status . #getApiT) (`shouldBe` InLedger)
            , expectListField 1 (#status . #getApiT) (`shouldBe` InLedger)
            , expectListField 2 (#status . #getApiT) (`shouldBe` InLedger)
            ]
        let getTime = view #time . fromJust . view #insertedAt
        pure $ Iso8601Time $ maximum $ getTime <$> getFromResponse id rTxs

    -- ACTION
<<<<<<< HEAD
    rTxsFromDate <- request @[ApiTransaction n] ctx
        (Link.listTransactions' @'Byron wSrc Nothing (Just start) Nothing Nothing)
        Default
        Empty

    rTxsAll <- request @[ApiTransaction n] ctx
        (Link.listTransactions' @'Byron wSrc Nothing Nothing Nothing Nothing)
=======
    rTxsFromDate <- request @[ApiTransaction] ctx
        (Link.listTransactions' @'Byron wSrc (Just start) Nothing Nothing)
        Default
        Empty

    rTxsAll <- request @[ApiTransaction] ctx
        (Link.listTransactions' @'Byron wSrc Nothing Nothing Nothing)
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
        Default
        Empty

    -- ASSERTIONS
    let outgoing t = (view (#direction. #getApiT) t) == Outgoing
    let txsFromDate, txsAllOutgoing :: [ApiTransaction]
        txsFromDate = getFromResponse id rTxsFromDate
        txsAllOutgoing = filter outgoing (getFromResponse id rTxsAll)

    verifyTxInputsAndOutputs txsFromDate
    verifyTxInputsAndOutputs txsAllOutgoing

    verify rTxsFromDate
        [ expectListSize 1
        , expectListField 0 #inputs (`shouldSatisfy` (all (isJust . view #source)))
        ]
  where
    title = "TRANS_REG_1670"
    verifyTxInputsAndOutputs
<<<<<<< HEAD
        :: forall (d :: NetworkDiscriminant).
            ( DecodeAddress d
            , DecodeStakeAddress d
            , EncodeAddress d
            , PaymentAddress d IcarusKey
            )
        => [ApiTransaction d]
=======
        :: [ApiTransaction]
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
        -> IO()
    verifyTxInputsAndOutputs transactions = do
        let inputs = view (#inputs)
        let outputs = view (#outputs)
        let amounts = getQuantity . view #amount . fromJust . view #source
        forM_ transactions $ \transaction -> do
            let inputAmounts = sum $ amounts <$> (inputs transaction)
            let outputAmounts = sum $ (getQuantity . view #amount) <$> (outputs transaction)
            inputAmounts `shouldSatisfy` (>= outputAmounts)

--
-- More Elaborated Fixtures
--

-- | Returns a source wallet and a list of payments.
--
-- NOTE: Random or Icarus wallets can be used interchangeably here.
<<<<<<< HEAD
fixtureCantCoverFee
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        )
    => Context t
=======
fixtureSingleUTxO
    :: Context t
    -> IO (ApiByronWallet, [Aeson.Value])
fixtureSingleUTxO ctx = do
    let n = _network ctx
    wSrc  <- fixtureRandomWalletWith ctx [1_000_000]
    addrs <- randomAddresses n . entropyToMnemonic <$> genEntropy
    let payments =
            [ mkPayment (head addrs) 100_000
            , mkPayment (head addrs) 100_000
            ]
    pure (wSrc, payments)

-- | Returns a source wallet and a list of payments. If submitted, the payments
-- should result in an error 403.
--
-- NOTE: Random or Icarus wallets can be used interchangeably here.
fixtureErrInputsDepleted
    :: Context t
    -> IO (ApiByronWallet, [Aeson.Value])
fixtureErrInputsDepleted ctx = do
    let n = _network ctx
    wSrc  <- fixtureRandomWalletWith ctx [12_000_000, 20_000_000, 17_000_000]
    addrs <- randomAddresses n . entropyToMnemonic <$> genEntropy
    let amnts = [40_000_000, 22, 22] :: [Natural]
    let payments = flip map (zip addrs amnts) $ uncurry (mkPayment)
    pure (wSrc, payments)

-- | Returns a source wallet and a list of payments.
--
-- NOTE: Random or Icarus wallets can be used interchangeably here.
fixtureCantCoverFee
    :: Context t
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
    -> IO (ApiByronWallet, [Aeson.Value])
fixtureCantCoverFee ctx = do
    let n = _network ctx
    let (feeMin, _) = ctx ^. #_feeEstimator $ PaymentDescription 1 1 1
    wSrc <- fixtureIcarusWalletWith ctx [feeMin `div` 2]
    addrs <- icarusAddresses n . entropyToMnemonic <$> genEntropy
    pure (wSrc, [mkPayment (head addrs) 1])

-- | Returns a source wallet and a list of payments.
--
-- NOTE: Random or Icarus wallets can be used interchangeably here.
fixtureNotEnoughMoney
<<<<<<< HEAD
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        )
    => Context t
=======
    :: Context t
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
    -> IO (ApiByronWallet, [Aeson.Value])
fixtureNotEnoughMoney ctx = do
    let n = _network ctx
    wSrc <- emptyIcarusWallet ctx
    addrs <- icarusAddresses n . entropyToMnemonic <$> genEntropy
    pure (wSrc, [mkPayment (head addrs) 1])

-- | Returns a source wallet and a list of payments.
--
-- NOTE: Random or Icarus wallets can be used interchangeably here.
fixtureWrongPassphrase
<<<<<<< HEAD
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        )
    => Context t
=======
    :: Context t
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
    -> IO (ApiByronWallet, [Aeson.Value])
fixtureWrongPassphrase ctx = do
    let n = _network ctx
    (wSrc, addrs) <- fixtureIcarusWalletAddrs n ctx
    pure (wSrc, [mkPayment (head addrs) 100_000])

-- | Returns a source wallet and a list of payments.
--
-- NOTE: Random or Icarus wallets can be used interchangeably here.
fixtureDeletedWallet
<<<<<<< HEAD
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        , PaymentAddress n ByronKey
        )
    => Context t
=======
    :: Context t
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
    -> IO (ApiByronWallet, [Aeson.Value])
fixtureDeletedWallet ctx = do
    let n = _network ctx
    wSrc <- emptyRandomWallet ctx
    _ <- request @() ctx (Link.deleteWallet @'Byron wSrc) Default Empty
    addr:_ <- randomAddresses n . entropyToMnemonic <$> genEntropy
    pure (wSrc, [mkPayment addr 100_000])

--
-- Helpers
--

-- | Construct a JSON payload for a single payment (address + amount)
mkPayment
    :: ApiAddress
    -> Natural
    -> Aeson.Value
mkPayment (ApiAddress addr) amnt = [json|
    { "address": #{addr}
    , "amount":
        { "quantity": #{amnt}
        , "unit": "lovelace"
        }
    } |]

postByronTransaction
<<<<<<< HEAD
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        )
    => Context t
=======
    :: Context t
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
        -- A surrounding API context
    -> ApiByronWallet
        -- ^ Source wallet
    -> [Aeson.Value]
        -- ^ A list of payment
    -> Text
        -- ^ Passphrase
    -> IO (HTTP.Status, Either RequestException ApiTransaction)
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
