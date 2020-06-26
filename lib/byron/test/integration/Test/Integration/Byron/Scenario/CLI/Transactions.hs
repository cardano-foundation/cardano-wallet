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

import Cardano.Mnemonic
    ( entropyToMnemonic, genEntropy )
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
import Data.Time.Utils
    ( utcTimePred, utcTimeSucc )
import Numeric.Natural
    ( Natural )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith
    , describe
    , it
    , pendingWith
    , shouldBe
    , shouldContain
    , shouldSatisfy
    )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , KnownCommand
    , Payload (..)
    , TxDescription (..)
    , between
    , deleteTransactionViaCLI
    , deleteWalletViaCLI
    , emptyIcarusWallet
    , emptyRandomWallet
    , eventually
    , expectCliField
    , expectCliListField
    , expectValidJSON
    , faucetAmt
    , fixtureIcarusWallet
    , fixtureIcarusWalletAddrs
    , fixtureIcarusWalletWith
    , fixturePassphrase
    , fixtureRandomWallet
    , fixtureRandomWalletAddrs
    , fixtureRandomWalletWith
    , getTransactionViaCLI
    , getTxId
    , getWalletViaCLI
    , icarusAddresses
    , listTransactionsViaCLI
    , postTransactionFeeViaCLI
    , postTransactionViaCLI
    , randomAddresses
    , request
    , unsafeGetTransactionTime
    , utcIso8601ToText
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( cmdOk
    , errMsg400StartTimeLaterThanEndTime
    , errMsg403Fee
    , errMsg403InputsDepleted
    , errMsg403NoPendingAnymore
    , errMsg403NotEnoughMoney_
    , errMsg403UTxO
    , errMsg403WrongPass
    , errMsg404CannotFindTx
    , errMsg404NoWallet
    , errMsg404NoWallet
    , falseWalletIds
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

    it "TRANS_LIST_01 - 0 txs on empty Byron wallet"
        $ \ctx -> forM_ [emptyRandomWallet, emptyIcarusWallet] $ \emptyByronWallet -> do
            w <- emptyByronWallet ctx
            (Exit code, Stdout out, Stderr err) <-
                listTransactionsViaCLI @t ctx [T.unpack $ w ^. walletId]
            err `shouldBe` cmdOk
            code `shouldBe` ExitSuccess
            list <- expectValidJSON (Proxy @([ApiTransaction n])) out
            length list `shouldBe` 0

    it "TRANS_LIST_01 - Can list transactions on Byron Wallet"
        $ \ctx -> forM_ [fixtureRandomWallet, fixtureIcarusWallet]
        $ \fixtureByronWallet -> do
            w <- fixtureByronWallet ctx
            (Exit code, Stdout out, Stderr err) <-
                listTransactionsViaCLI @t ctx [T.unpack $ w ^. walletId]
            err `shouldBe` cmdOk
            code `shouldBe` ExitSuccess
            list <- expectValidJSON (Proxy @([ApiTransaction n])) out
            length list `shouldBe` 10

    it "TRANS_LIST_03 - Can order results"
        $ \ctx -> forM_ [fixtureRandomWalletWith @n, fixtureIcarusWalletWith @n]
        $ \fixtureByronWalletWith -> do
            let a1 = Quantity $ sum $ replicate 10 1
            let a2 = Quantity $ sum $ replicate 10 2
            w <- fixtureByronWalletWith ctx $ mconcat
                    [ replicate 10 1
                    , replicate 10 2
                    ]

            let orderings =
                    [ ( mempty
                      , [ expectCliListField 0 #amount (`shouldBe` a2)
                        , expectCliListField 1 #amount (`shouldBe` a1)
                        ]
                      )
                    , ( [ "--order", "ascending" ]
                      , [ expectCliListField 0 #amount (`shouldBe` a1)
                        , expectCliListField 1 #amount (`shouldBe` a2)
                        ]
                      )
                    , ( [ "--order", "descending" ]
                      , [ expectCliListField 0 #amount (`shouldBe` a2)
                        , expectCliListField 1 #amount (`shouldBe` a1)
                        ]
                      )
                    ]

            forM_ orderings $ \(order, expects) -> do
                let args = T.unpack <$> w ^. walletId : order
                (Exit code, Stdout out, Stderr err) <-
                    listTransactionsViaCLI @t ctx args
                err `shouldBe` cmdOk
                code `shouldBe` ExitSuccess
                outJson <- expectValidJSON (Proxy @([ApiTransaction n])) out
                length outJson `shouldBe` 2
                verify outJson expects

    describe "TRANS_LIST_04 - Faulty start, end, order values" $ do
        let orderErr = "Please specify one of the following values:\
            \ ascending, descending."
        let startEndErr = "Expecting ISO 8601 date-and-time format\
            \ (basic or extended), e.g. 2012-09-25T10:15:00Z."
        let queries  =
                [ ( [ "--start", "2009" ], startEndErr )
                , ( [ "--start", "2012-09-25T10:15:00Z", "--end",  "2016-11-21"]
                  , startEndErr
                  )
                , ( [ "--start", "2012-09-25", "--end",  "2016-11-21T10:15:00Z"]
                  , startEndErr
                  )
                , ( [ "--start", "2012-09-25T10:15:00Z", "--end",  "2016-11-21"]
                  , startEndErr
                  )
                , ( [ "--order", "scending" ], orderErr )
                , ( [ "--start", "2012-09-25T10:15:00Z", "--order", "asc" ]
                  , orderErr
                  )
                , ( [ "--start", "2009-09-09T09:09:09Z"
                    , "--end",  "2001-01-01T01:01:01Z"]
                  , errMsg400StartTimeLaterThanEndTime
                        "2009-09-09T09:09:09Z" "2001-01-01T01:01:01Z"
                  )
                ]
        forM_ queries $ \(query, message) -> it (unwords query)
            $ \ctx -> forM_ [emptyRandomWallet, emptyIcarusWallet]
            $ \emptyByronWallet -> do
                w <- emptyByronWallet ctx
                (Exit code, Stdout out, Stderr err) <-
                    listTransactionsViaCLI @t ctx (T.unpack (w ^. walletId) : query)
                err `shouldContain` message
                code `shouldBe` ExitFailure 1
                out `shouldBe` mempty

    it "TRANS_LIST_04 - Deleted wallet"
        $ \ctx -> forM_ [emptyRandomWallet, emptyIcarusWallet]
        $ \emptyByronWallet -> do
            w <- emptyByronWallet ctx
            Exit cd <- deleteWalletViaCLI @t ctx $ T.unpack (w ^. walletId)
            cd `shouldBe` ExitSuccess
            (Exit code, Stdout out, Stderr err) <-
                listTransactionsViaCLI @t ctx [T.unpack $ w ^. walletId]
            err `shouldContain` errMsg404NoWallet (w ^. walletId)
            code `shouldBe` ExitFailure 1
            out `shouldBe` mempty

    it "TRANS_LIST_RANGE_01 - \
       \Transaction at time t is SELECTED by small ranges that cover it"
       $ \ctx -> forM_ [fixtureRandomWalletWith @n, fixtureIcarusWalletWith @n]
       $ \fixtureByronWalletWith -> do
              w <- fixtureByronWalletWith ctx [1]
              let walId = w ^. walletId
              Stdout o  <- listTransactionsViaCLI @t ctx [ T.unpack walId ]
              oJson <- expectValidJSON (Proxy @([ApiTransaction n])) o
              let t = unsafeGetTransactionTime oJson
              let (te, tl) = (utcTimePred t, utcTimeSucc t)
              let query t1 t2 =
                        [ "--start", utcIso8601ToText t1
                        , "--end", utcIso8601ToText t2
                        ]
              Stdout o1  <- listTransactionsViaCLI @t ctx
                    ( T.unpack <$> walId : (query t t) )
              Stdout o2 <- listTransactionsViaCLI @t ctx
                    ( T.unpack <$> walId : (query te t) )
              Stdout o3 <- listTransactionsViaCLI @t ctx
                    ( T.unpack <$> walId : (query t tl) )
              Stdout o4 <- listTransactionsViaCLI @t ctx
                    ( T.unpack <$> walId : (query te tl) )
              oJson1 <- expectValidJSON (Proxy @([ApiTransaction n])) o1
              oJson2 <- expectValidJSON (Proxy @([ApiTransaction n])) o2
              oJson3 <- expectValidJSON (Proxy @([ApiTransaction n])) o3
              oJson4 <- expectValidJSON (Proxy @([ApiTransaction n])) o4
              length <$> [oJson1, oJson2, oJson3, oJson4] `shouldSatisfy` all (== 1)

    it "TRANS_LIST_RANGE_02 - \
       \Transaction at time t is NOT selected by range [t + 𝛿t, ...)"
       $ \ctx -> forM_ [fixtureRandomWalletWith @n, fixtureIcarusWalletWith @n]
       $ \fixtureByronWalletWith -> do
              w <- fixtureByronWalletWith ctx [1]
              let walId = w ^. walletId
              Stdout o  <- listTransactionsViaCLI @t ctx [ T.unpack walId ]
              oJson <- expectValidJSON (Proxy @([ApiTransaction n])) o
              let t = unsafeGetTransactionTime oJson
              let tl = utcIso8601ToText $ utcTimeSucc t
              Stdout o1  <- listTransactionsViaCLI @t ctx
                    ( T.unpack <$> [walId, "--start", tl] )
              Stdout o2 <- listTransactionsViaCLI @t ctx
                    ( T.unpack <$> [walId, "--start", tl, "--end", tl] )
              oJson1 <- expectValidJSON (Proxy @([ApiTransaction n])) o1
              oJson2 <- expectValidJSON (Proxy @([ApiTransaction n])) o2
              length <$> [oJson1, oJson2] `shouldSatisfy` all (== 0)

    it "TRANS_LIST_RANGE_03 - \
       \Transaction at time t is NOT selected by range (..., t - 𝛿t]"
       $ \ctx -> forM_ [fixtureRandomWalletWith @n, fixtureIcarusWalletWith @n]
       $ \fixtureByronWalletWith -> do
              w <- fixtureByronWalletWith ctx [1]
              let walId = w ^. walletId
              Stdout o  <- listTransactionsViaCLI @t ctx [ T.unpack walId ]
              oJson <- expectValidJSON (Proxy @([ApiTransaction n])) o
              let t = unsafeGetTransactionTime oJson
              let te = utcIso8601ToText $ utcTimePred t
              Stdout o1  <- listTransactionsViaCLI @t ctx
                      ( T.unpack <$> [walId, "--end", te] )
              Stdout o2 <- listTransactionsViaCLI @t ctx
                      ( T.unpack <$> [walId, "--start", te, "--end", te] )
              oJson1 <- expectValidJSON (Proxy @([ApiTransaction n])) o1
              oJson2 <- expectValidJSON (Proxy @([ApiTransaction n])) o2
              length <$> [oJson1, oJson2] `shouldSatisfy` all (== 0)

    it "TRANS_DELETE_01a - Can forget pending tx, still it resolves when it is OK"
        $ \ctx ->forM_ [fixtureRandomWalletAddrs @n, fixtureRandomWalletAddrs @n]
        $ \fixtureByronWallet -> do
        pendingWith
            "This test is built on a race-condition. Should the transaction be \
            \inserted and discovered before the forget request is sent, it \
            \fails. There are probably better ways of testing this..."

        --SETUP
        let amnt = 100_000 :: Natural
        (wSrc, addrs) <- fixtureByronWallet ctx
        let payment = mkPaymentCmd @n (head addrs) amnt
        let wSrcId = T.unpack (wSrc ^. walletId)

        -- post transaction
        let args = T.unpack <$> ((wSrc ^. walletId) : payment)
        (c, out, err) <- postTransactionViaCLI @t ctx (T.unpack fixturePassphrase) args
        err `shouldBe` "Please enter your passphrase: **************\nOk.\n"
        c `shouldBe` ExitSuccess
        txJson <- expectValidJSON (Proxy @(ApiTransaction n)) out

        -- Try Forget transaction once it's no longer pending
        let txId =  getTxId txJson
        (Exit c2, Stdout out2, Stderr err2) <-
            deleteTransactionViaCLI @t ctx wSrcId txId
        c2 `shouldBe` ExitSuccess
        err2 `shouldContain` cmdOk
        out2 `shouldBe` "\n"

        eventually "Tx is in ledger" $ do
            (fromStdout <$> listTransactionsViaCLI @t ctx [wSrcId])
                >>= expectValidJSON (Proxy @([ApiTransaction n]))
                >>= flip verify
                    [ expectCliListField 0
                        (#direction . #getApiT) (`shouldBe` Outgoing)
                    , expectCliListField 0
                        (#status . #getApiT) (`shouldBe` InLedger)
                    ]

    it "TRANS_DELETE_01b - Cannot forget pending transaction when not pending anymore via CLI"
        $ \ctx -> forM_ [fixtureRandomWalletAddrs @n, fixtureRandomWalletAddrs @n]
        $ \fixtureByronWallet -> do
        --SETUP
        let amnt = 100_000 :: Natural
        (wSrc, addrs) <- fixtureByronWallet ctx
        let payment = mkPaymentCmd @n (head addrs) amnt
        let wSrcId = T.unpack (wSrc ^. walletId)

        -- post transaction
        let args = T.unpack <$> ((wSrc ^. walletId) : payment)
        (c, out, err) <- postTransactionViaCLI @t ctx (T.unpack fixturePassphrase) args
        err `shouldBe` "Please enter your passphrase: **************\nOk.\n"
        c `shouldBe` ExitSuccess
        txJson <- expectValidJSON (Proxy @(ApiTransaction n)) out

        eventually "Tx is in ledger" $ do
            (fromStdout <$> listTransactionsViaCLI @t ctx [wSrcId])
                >>= expectValidJSON (Proxy @([ApiTransaction n]))
                >>= flip verify
                    [ expectCliListField 0
                        (#direction . #getApiT) (`shouldBe` Outgoing)
                    , expectCliListField 0
                        (#status . #getApiT) (`shouldBe` InLedger)
                    ]

        -- Try Forget transaction once it's no longer pending
        let txId =  getTxId txJson
        (Exit c2, Stdout out2, Stderr err2) <-
            deleteTransactionViaCLI @t ctx wSrcId txId
        err2 `shouldContain` errMsg403NoPendingAnymore (T.pack txId)
        out2 `shouldBe` ""
        c2 `shouldBe` ExitFailure 1

    it "TRANS_DELETE_03 - Cannot forget tx that is not found via CLI"
        $ \ctx -> forM_ [emptyRandomWallet, emptyIcarusWallet]
        $ \emptyByronWallet -> do
            wSrc <- emptyByronWallet ctx
            let wid = T.unpack (wSrc ^. walletId)
            let txId = "3e6ec12da4414aa0781ff8afa9717ae53ee8cb4aa55d622f65bc62619a4f7b12"
            -- forget transaction
            (Exit c, Stdout out, Stderr err) <-
                deleteTransactionViaCLI @t ctx wid (T.unpack txId)
            err `shouldContain` errMsg404CannotFindTx txId
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1

    describe "TRANS_DELETE_04 - False wallet ids via CLI" $ do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> do
            let txId = "3e6ec12da4414aa0781ff8afa9717ae53ee8cb4aa55d622f65bc62619a4f7b12"
            -- forget transaction once again
            (Exit c, Stdout out, Stderr err) <-
                deleteTransactionViaCLI @t ctx walId (T.unpack txId)
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1
            if (title == "40 chars hex") then
                err `shouldContain` "I couldn't find a wallet with \
                    \the given id: 1111111111111111111111111111111111111111"
            else
                err `shouldContain` "wallet id should be a \
                    \hex-encoded string of 40 characters"

    it "TRANS_DELETE_06 -\
        \ Cannot forget tx that is performed from different wallet via CLI"
        $ \ctx -> forM_ [fixtureRandomWalletAddrs @n, fixtureRandomWalletAddrs @n]
        $ \fixtureByronWallet -> do
        --SETUP
        let amnt = 100_000 :: Natural
        (wSrc, addrs) <- fixtureByronWallet ctx
        let payment = mkPaymentCmd @n (head addrs) amnt

        -- post transaction
        let args = T.unpack <$> ((wSrc ^. walletId) : payment)
        (c, out, err) <- postTransactionViaCLI @t ctx (T.unpack fixturePassphrase) args
        err `shouldBe` "Please enter your passphrase: **************\nOk.\n"
        c `shouldBe` ExitSuccess
        txJson <- expectValidJSON (Proxy @(ApiTransaction n)) out

        -- Try Forget transaction using different wallet
        let txId =  getTxId txJson
        wDiff <- emptyRandomWallet ctx
        let widDiff = T.unpack (wDiff ^. walletId)
        (Exit c2, Stdout out2, Stderr err2) <-
            deleteTransactionViaCLI @t ctx widDiff txId
        err2 `shouldContain` errMsg404CannotFindTx (T.pack txId)
        out2 `shouldBe` ""
        c2 `shouldBe` ExitFailure 1

    describe "TRANS_DELETE_07 - invalid tx id via CLI" $ do
        let txIds =
                [ replicate 63 '1'
                , replicate 65 '1'
                , replicate 64 'ś'
                ]
        forM_ txIds $ \tid -> it (show tid) $ \ctx -> do
            w <- emptyRandomWallet ctx
            let wid = T.unpack (w ^. walletId)
            (Exit c, Stdout out, Stderr err) <-
                deleteTransactionViaCLI @t ctx wid tid
            err `shouldContain`
                "should be a hex-encoded string of 64 characters"
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1

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
    let txId =  getTxId r

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

            -- Verify Tx in dest wallet is Incoming and InLedger
            (Exit code1, Stdout out1, Stderr err1) <-
                getTransactionViaCLI @t ctx (T.unpack (wDest ^. walletId)) txId
            err1 `shouldBe` "Ok.\n"
            code1 `shouldBe` ExitSuccess
            outJson1 <- expectValidJSON (Proxy @(ApiTransaction n)) out1
            verify outJson1
                [ expectCliField (#direction . #getApiT) (`shouldBe` Incoming)
                , expectCliField (#status . #getApiT) (`shouldBe` InLedger)
                ]

    -- Verify Tx in source wallet is Outgoing and InLedger
    (Exit code2, Stdout out2, Stderr err2) <-
        getTransactionViaCLI @t ctx (T.unpack (wSrc ^. walletId)) txId
    err2 `shouldBe` "Ok.\n"
    code2 `shouldBe` ExitSuccess
    outJson2 <- expectValidJSON (Proxy @(ApiTransaction n)) out2
    verify outJson2
        [ expectCliField (#direction . #getApiT) (`shouldBe` Outgoing)
        , expectCliField (#status . #getApiT) (`shouldBe` InLedger)
        ]
  where
    title = "CLI_TRANS_CREATE_01/02, CLI_TRANS_GET_01 - " ++ show n ++ " recipient(s)"
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
    err `shouldBe` cmdOk
    c `shouldBe` ExitSuccess
    r <- expectValidJSON (Proxy @ApiFee) out
    let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
            { nInputs  = length fixtures
            , nOutputs = length fixtures
            , nChanges = length fixtures
            }
    verify r
        [ expectCliField #estimatedMin $
            between (Quantity feeMin, Quantity feeMax)
        ]
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
    err `shouldBe` cmdOk
    c `shouldBe` ExitSuccess
    r <- expectValidJSON (Proxy @ApiFee) out
    let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
            { nInputs  = 1
            , nOutputs = 1
            , nChanges = 0
            }
    verify r
        [ expectCliField #estimatedMin $
            between (Quantity feeMin, Quantity feeMax)
        ]
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
