{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Byron.Scenario.CLI.Transactions
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
    ( NetworkDiscriminant (..), PaymentAddress (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.Mnemonic
    ( entropyToMnemonic, genEntropy )
import Cardano.Wallet.Primitive.Types
    ( Address, Direction (..), TxStatus (..) )
import Control.Monad
    ( forM, forM_, join )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Numeric.Natural
    ( Natural )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, describe, it, shouldBe, shouldContain )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , KnownCommand
    , Payload (..)
    , TxDescription (..)
    , between
    , emptyIcarusWallet
    , emptyRandomWallet
    , eventually
    , expectCliField
    , expectValidJSON
    , faucetAmt
    , fixtureIcarusWallet
    , fixtureIcarusWalletAddrs
    , fixtureIcarusWalletWith
    , fixturePassphrase
    , fixtureRandomWallet
    , fixtureRandomWalletAddrs
    , fixtureRandomWalletWith
    , getWalletViaCLI
    , icarusAddresses
    , postTransactionFeeViaCLI
    , postTransactionViaCLI
    , randomAddresses
    , request
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( errMsg403Fee
    , errMsg403InputsDepleted
    , errMsg403NotEnoughMoney_
    , errMsg403UTxO
    , errMsg403WrongPass
    , errMsg404NoWallet
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Text as T

spec
    :: forall (n :: NetworkDiscriminant) t.
        ( PaymentAddress n IcarusKey
        , PaymentAddress n ByronKey
        , EncodeAddress n
        , KnownCommand t
        , DecodeAddress n
        )
    => SpecWith (Context t)
spec = describe "BYRON_TXS_CLI" $ do
    -- Random → Random
    scenario_TRANS_CREATE_01_02 @n fixtureRandomWallet
        [ fixtureRandomWalletAddrs @n ]

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

--
-- Scenarios
--

scenario_TRANS_CREATE_01_02
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , KnownCommand t
        )
    => (Context t -> IO ApiByronWallet)
    -> [(Context t -> IO (ApiByronWallet, [Address]))]
    -> SpecWith (Context t)
scenario_TRANS_CREATE_01_02 fixtureSource fixtures = it title $ \ctx -> do
    -- SETUP
    let amnt = 100_000 :: Natural
    wSrc <- fixtureSource ctx
    (recipients, payments) <- fmap unzip $ forM fixtures $ \fixtureDest -> do
        (wDest, addrs) <- fixtureDest ctx
        pure (wDest, (mkPaymentCmd @n (head addrs) amnt))

    -- ACTION
    let args = T.unpack <$> ((wSrc ^. walletId) : (join payments))
    (c, out, err) <- postTransactionViaCLI @t ctx (T.unpack fixturePassphrase) args

    -- ASSERTIONS
    err `shouldBe` "Please enter your passphrase: **************\nOk.\n"
    c `shouldBe` ExitSuccess
    r <- expectValidJSON (Proxy @(ApiTransaction n)) out
    let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
            { nInputs  =  fromIntegral n
            , nOutputs =  fromIntegral n
            , nChanges =  fromIntegral n
            }
    verify r
        [ expectCliField #amount $ between
              ( Quantity (feeMin + n * amnt)
              , Quantity (feeMax + n * amnt)
              )
        , expectCliField #direction (`shouldBe` ApiT Outgoing)
        , expectCliField #status (`shouldBe` ApiT Pending)
        ]

    eventually "source balance decreases" $ do
        Stdout outSrc <- getWalletViaCLI @t ctx
            (T.unpack (wSrc ^. walletId))
        rSrc <- expectValidJSON (Proxy @ApiByronWallet) outSrc
        verify rSrc
            [ expectCliField (#balance . #available) $ between
                ( Quantity (faucetAmt - (n * amnt) - feeMax)
                , Quantity (faucetAmt - (n * amnt) - feeMin)
                )
            ]

    forM_ recipients $ \wDest -> do
        eventually "destination balance increases" $ do
            Stdout outDest <- getWalletViaCLI @t ctx
                (T.unpack (wDest ^. walletId))
            rDest <- expectValidJSON (Proxy @ApiByronWallet) outDest
            verify rDest
                [ expectCliField (#balance . #available)
                    (`shouldBe` Quantity (faucetAmt + amnt))
                ]
  where
    title = "CLI_TRANS_CREATE_01/02 - " ++ show n ++ " recipient(s)"
    n = fromIntegral $ length fixtures

scenario_TRANS_ESTIMATE_01_02
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , KnownCommand t
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
        pure $ mkPaymentCmd @n (head addrs) amnt

    -- ACTION
    let args = T.unpack <$> ((wSrc ^. walletId) : (join payments))
    (Exit c, Stdout out, Stderr err) <- postTransactionFeeViaCLI @t ctx args

    -- ASSERTIONS
    err `shouldBe` "Ok.\n"
    c `shouldBe` ExitSuccess
    r <- expectValidJSON (Proxy @ApiFee) out
    let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
            { nInputs  = length fixtures
            , nOutputs = length fixtures
            , nChanges = length fixtures
            }
    verify r
        [ expectCliField #amount $ between (Quantity feeMin, Quantity feeMax) ]
  where
    title = "CLI_TRANS_ESTIMATE_01/02 - " ++ show (length fixtures) ++ " recipient(s)"

scenario_TRANS_CREATE_02x
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n ByronKey
        , KnownCommand t
        )
    => SpecWith (Context t)
scenario_TRANS_CREATE_02x = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureSingleUTxO @n ctx

    -- ACTION
    let args = T.unpack <$> ((wSrc ^. walletId) : payments)
    (c, out, err) <- postTransactionViaCLI @t ctx (T.unpack fixturePassphrase) args

    -- ASSERTIONS
    T.unpack err `shouldContain` errMsg403UTxO
    c `shouldBe` ExitFailure 1
    out `shouldBe` mempty

  where
    title = "CLI_TRANS_CREATE_02x - Multi-output failure w/ single UTxO"

scenario_TRANS_CREATE_04a
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n ByronKey
        , KnownCommand t
        )
    => SpecWith (Context t)
scenario_TRANS_CREATE_04a = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureErrInputsDepleted @n ctx

    -- ACTION
    let args = T.unpack <$> ((wSrc ^. walletId) : payments)
    (c, out, err) <- postTransactionViaCLI @t ctx (T.unpack fixturePassphrase) args

    -- ASSERTIONS
    T.unpack err `shouldContain` errMsg403InputsDepleted
    c `shouldBe` ExitFailure 1
    out `shouldBe` mempty
  where
    title = "CLI_TRANS_CREATE_04 - Error shown when ErrInputsDepleted encountered"

scenario_TRANS_CREATE_04b
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        , KnownCommand t
        )
    => SpecWith (Context t)
scenario_TRANS_CREATE_04b = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureCantCoverFee @n ctx

    -- ACTION
    let args = T.unpack <$> ((wSrc ^. walletId) : payments)
    (c, out, err) <- postTransactionViaCLI @t ctx (T.unpack fixturePassphrase) args

    -- ASSERTIONS
    T.unpack err `shouldContain` errMsg403Fee
    c `shouldBe` ExitFailure 1
    out `shouldBe` mempty
  where
    title = "CLI_TRANS_CREATE_04 - Can't cover fee"

scenario_TRANS_CREATE_04c
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        , KnownCommand t
        )
    => SpecWith (Context t)
scenario_TRANS_CREATE_04c = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureNotEnoughMoney @n ctx

    -- ACTION
    let args = T.unpack <$> ((wSrc ^. walletId) : payments)
    (c, out, err) <- postTransactionViaCLI @t ctx (T.unpack fixturePassphrase) args

    -- ASSERTIONS
    T.unpack err `shouldContain` errMsg403NotEnoughMoney_
    c `shouldBe` ExitFailure 1
    out `shouldBe` mempty
  where
    title = "CLI_TRANS_CREATE_04 - Not enough money"

scenario_TRANS_CREATE_04d
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        , KnownCommand t
        )
    => SpecWith (Context t)
scenario_TRANS_CREATE_04d = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureWrongPassphrase @n ctx

    -- ACTION
    let args = T.unpack <$> ((wSrc ^. walletId) : payments)
    (c, out, err) <- postTransactionViaCLI @t ctx "This passphrase is wrong" args

    -- ASSERTIONS
    T.unpack err `shouldContain` errMsg403WrongPass
    c `shouldBe` ExitFailure 1
    out `shouldBe` mempty
  where
    title = "CLI_TRANS_CREATE_04 - Wrong password"

scenario_TRANS_ESTIMATE_04a
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n ByronKey
        , KnownCommand t
        )
    => SpecWith (Context t)
scenario_TRANS_ESTIMATE_04a = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureErrInputsDepleted @n ctx

    -- ACTION
    let args = T.unpack <$> ((wSrc ^. walletId) : payments)
    (Exit c, Stdout out, Stderr err) <- postTransactionFeeViaCLI @t ctx args

    -- ASSERTIONS
    err `shouldContain` errMsg403InputsDepleted
    c `shouldBe` ExitFailure 1
    out `shouldBe` mempty
  where
    title = "CLI_TRANS_ESTIMATE_04 - Error shown when ErrInputsDepleted encountered"

scenario_TRANS_ESTIMATE_04b
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        , KnownCommand t
        )
    => SpecWith (Context t)
scenario_TRANS_ESTIMATE_04b = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureCantCoverFee @n ctx

    -- ACTION
    let args = T.unpack <$> ((wSrc ^. walletId) : payments)
    (Exit c, Stdout out, Stderr err) <- postTransactionFeeViaCLI @t ctx args

    -- ASSERTIONS
    err `shouldBe` "Ok.\n"
    c `shouldBe` ExitSuccess
    r <- expectValidJSON (Proxy @ApiFee) out
    let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
            { nInputs  = 1
            , nOutputs = 1
            , nChanges = 0
            }
    verify r
        [ expectCliField #amount $ between (Quantity feeMin, Quantity feeMax) ]
  where
    title = "CLI_TRANS_ESTIMATE_04 - Can't cover fee"

scenario_TRANS_ESTIMATE_04c
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n IcarusKey
        , KnownCommand t
        )
    => SpecWith (Context t)
scenario_TRANS_ESTIMATE_04c = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureNotEnoughMoney @n ctx

    -- ACTION
    let args = T.unpack <$> ((wSrc ^. walletId) : payments)
    (Exit c, Stdout out, Stderr err) <- postTransactionFeeViaCLI @t ctx args

    -- ASSERTIONS
    err `shouldContain` errMsg403NotEnoughMoney_
    c `shouldBe` ExitFailure 1
    out `shouldBe` mempty
  where
    title = "CLI_TRANS_ESTIMATE_04 - Not enough money"

scenario_TRANS_CREATE_07
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , EncodeAddress n
        , PaymentAddress n ByronKey
        , KnownCommand t
        )
    => SpecWith (Context t)
scenario_TRANS_CREATE_07 = it title $ \ctx -> do
    -- SETUP
    (wSrc, payments) <- fixtureDeletedWallet @n ctx

    -- ACTION
    let args = T.unpack <$> ((wSrc ^. walletId) : payments)
    (c, out, err) <- postTransactionViaCLI @t ctx (T.unpack fixturePassphrase) args

    -- ASSERTIONS
    T.unpack err `shouldContain` errMsg404NoWallet (wSrc ^. walletId)
    c `shouldBe` ExitFailure 1
    out `shouldBe` mempty

  where
    title = "CLI_TRANS_CREATE_07 - Deleted wallet"

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
    -> IO (ApiByronWallet, [Text])
fixtureSingleUTxO ctx = do
    wSrc  <- fixtureRandomWalletWith @n ctx [1_000_000]
    addrs <- randomAddresses @n . entropyToMnemonic <$> genEntropy
    let addrStr = encodeAddress @n (head addrs)
    let payments =
            [ "--payment", "100000@" <> addrStr
            , "--payment", "100000@" <> addrStr
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
    -> IO (ApiByronWallet, [Text])
fixtureErrInputsDepleted ctx = do
    wSrc  <- fixtureRandomWalletWith @n ctx [12_000_000, 20_000_000, 17_000_000]
    addrs <- randomAddresses @n . entropyToMnemonic <$> genEntropy
    -- let addrStrs = encodeAddress @n <$> (addrs)
    let amnts = [40_000_000, 22, 22] :: [Natural]
    let payments = flip map (zip addrs amnts) $ uncurry (mkPaymentCmd @n)
    pure (wSrc, join payments)

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
    -> IO (ApiByronWallet, [Text])
fixtureCantCoverFee ctx = do
    let (feeMin, _) = ctx ^. #_feeEstimator $ PaymentDescription 1 1 1
    wSrc <- fixtureIcarusWalletWith @n ctx [feeMin `div` 2]
    addrs <- icarusAddresses @n . entropyToMnemonic <$> genEntropy
    pure (wSrc, join [mkPaymentCmd @n (head addrs) 1])

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
    -> IO (ApiByronWallet, [Text])
fixtureNotEnoughMoney ctx = do
    wSrc <- emptyIcarusWallet ctx
    addrs <- icarusAddresses @n . entropyToMnemonic <$> genEntropy
    pure (wSrc, mkPaymentCmd @n (head addrs) 1)

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
    -> IO (ApiByronWallet, [Text])
fixtureWrongPassphrase ctx = do
    (wSrc, addrs) <- fixtureIcarusWalletAddrs @n ctx
    pure (wSrc, mkPaymentCmd @n (head addrs) 100_000)

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
    -> IO (ApiByronWallet, [Text])
fixtureDeletedWallet ctx = do
    wSrc <- emptyRandomWallet ctx
    _ <- request @() ctx (Link.deleteWallet @'Byron wSrc) Default Empty
    addrs <- randomAddresses @n . entropyToMnemonic <$> genEntropy
    pure (wSrc, mkPaymentCmd @n (head addrs) 100_000)

--
-- Helpers
--
-- | Construct a Cmd param for a single payment (address + amount)
mkPaymentCmd
    :: forall (n :: NetworkDiscriminant).
        ( EncodeAddress n
        )
    => Address
    -> Natural
    -> [Text]
mkPaymentCmd addr_ amnt = ["--payment", T.pack (show amnt) <> "@" <> addr]
  where
    addr = encodeAddress @n addr_
