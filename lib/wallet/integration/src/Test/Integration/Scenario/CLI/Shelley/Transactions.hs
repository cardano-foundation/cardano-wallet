{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.CLI.Shelley.Transactions
    ( spec
    ) where

import Prelude

import Cardano.CLI
    ( Port )
import Cardano.Wallet.Api.Types
    ( ApiFee (..), ApiTransaction, ApiWallet, apiAddress )
import Cardano.Wallet.Api.Types.SchemaMetadata
    ( TxMetadataSchema (..), detailedMetadata, noSchemaMetadata )
import Cardano.Wallet.Primitive.Types
    ( SortOrder (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..), TxMetadata (..), TxMetadataValue (..), TxStatus (..) )
import Cardano.Wallet.Read.NetworkId
    ( HasSNetworkId (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( encodeAddress )
import Control.Monad
    ( forM_, join )
import Control.Monad.IO.Class
    ( MonadIO )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO (..) )
import Control.Monad.Trans.Resource
    ( ResourceT, runResourceT )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Generics.Product.Typed
    ( typed )
import Data.List.Extra
    ( enumerate )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( showT )
import Data.Time.Utils
    ( utcTimePred, utcTimeSucc )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, describe )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , between
    , cardanoWalletCLI
    , deleteTransactionViaCLI
    , deleteWalletViaCLI
    , emptyRandomWallet
    , emptyWallet
    , eventually
    , expectCliField
    , expectCliListField
    , expectValidJSON
    , faucetAmt
    , fixturePassphrase
    , fixtureWallet
    , fixtureWalletWith
    , getTransactionViaCLI
    , getTxId
    , getWalletViaCLI
    , listAddresses
    , listAllTransactions
    , listTransactionsViaCLI
    , minUTxOValue
    , postTransactionFeeViaCLI
    , postTransactionViaCLI
    , unsafeGetTransactionTime
    , utcIso8601ToText
    , verify
    , walletId
    , (.>=)
    )
import Test.Integration.Framework.TestData
    ( arabicWalletName
    , errMsg403AlreadyInLedger
    , errMsg403WrongPass
    , errMsg404CannotFindTx
    , errMsg404NoWallet
    , falseWalletIds
    , invalidByronBase58
    , kanjiWalletName
    , polishWalletName
    , wildcardsWalletName
    )
import UnliftIO.Exception
    ( throwString )

import qualified Data.Map as Map
import qualified Data.Text as T

spec
    :: forall n
     . HasSNetworkId n
    => SpecWith Context
spec = describe "SHELLEY_CLI_TRANSACTIONS" $ do
    it "TRANS_CREATE_01 - Can create transaction via CLI" $ \ctx -> runResourceT $ do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx

        let amt = fromIntegral . minUTxOValue . _mainEra $ ctx
        args <- postTxArgs ctx wSrc wDest amt Nothing Nothing
        Stdout feeOut <- postTransactionFeeViaCLI ctx args
        ApiFee (Quantity feeMin) (Quantity feeMax) _ (Quantity 0) <-
            expectValidJSON Proxy feeOut

        txJson <- postTxViaCLI ctx wSrc wDest amt Nothing Nothing
        verify txJson
            [ expectCliField (#amount . #getQuantity)
                (between (feeMin + amt, feeMax + amt))
            , expectCliField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectCliField (#status . #getApiT) (`shouldBe` Pending)
            , expectCliField #metadata (`shouldBe` Nothing)
            ]

        -- verify balance on src wallet
        Stdout gOutSrc <- getWalletViaCLI ctx (T.unpack (wSrc ^. walletId))
        gJson <- expectValidJSON (Proxy @ApiWallet) gOutSrc
        verify gJson
            [ expectCliField
                (#balance . #total)
                (.>= Quantity (faucetAmt - feeMax - amt))
            ]

        eventually "balance on dest wallet is OK" $ do
            Stdout gOutDest <- getWalletViaCLI ctx
                (T.unpack (wDest ^. walletId))
            destJson <- expectValidJSON (Proxy @ApiWallet) gOutDest
            verify destJson
                [ expectCliField
                        (#balance . #available) (`shouldBe` Quantity amt)
                , expectCliField
                        (#balance . #total) (`shouldBe` Quantity amt)
                ]

    it "TRANS_CREATE_02 - Multiple Output Tx to single wallet via CLI" $ \ctx -> runResourceT $ do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addr <- listAddresses @n ctx wDest
        let addr1 = encodeAddress (sNetworkId @n) (apiAddress $ addr !! 1 ^. #id)
        let addr2 = encodeAddress (sNetworkId @n) (apiAddress $ addr !! 2 ^. #id)
        let amt = fromIntegral . minUTxOValue . _mainEra $ ctx
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addr1
                , "--payment", T.pack (show amt) <> "@" <> addr2
                ]

        Stdout feeOut <- postTransactionFeeViaCLI ctx args
        ApiFee (Quantity feeMin) (Quantity feeMax) _ _ <- expectValidJSON Proxy feeOut

        -- post transaction
        (c, out, err) <- postTransactionViaCLI ctx "cardano-wallet" args
        err `shouldBe` "Please enter your passphrase: **************\nOk.\n"
        txJson <- expectValidJSON (Proxy @(ApiTransaction n)) out
        verify txJson
            [ expectCliField
                (#amount . #getQuantity)
                (between (feeMin + (2*amt), feeMax + (2*amt)))
            , expectCliField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectCliField (#status . #getApiT) (`shouldBe` Pending)
            ]
        c `shouldBe` ExitSuccess

        -- verify balance on src wallet
        Stdout gOutSrc <- getWalletViaCLI ctx (T.unpack (wSrc ^. walletId))
        gJson <- expectValidJSON (Proxy @ApiWallet) gOutSrc
        verify gJson
            [ expectCliField
                (#balance . #total)
                (.>= Quantity (faucetAmt - feeMax - (2*amt)))
            ]

        eventually "balance on dest wallet is OK" $ do
            Stdout gOutDest <- getWalletViaCLI ctx
                (T.unpack (wDest ^. walletId))
            destJson <- expectValidJSON (Proxy @ApiWallet) gOutDest
            verify destJson
                [ expectCliField
                    (#balance . #available) (`shouldBe` Quantity (2*amt))
                , expectCliField
                    (#balance . #total) (`shouldBe` Quantity (2*amt))
                ]

    it "TRANS_CREATE_04 - Wrong password" $ \ctx -> runResourceT $ do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses @n ctx wDest
        let addr = encodeAddress (sNetworkId @n) (apiAddress $ addrs ^. #id)
        let amt = T.pack . show . minUTxOValue . _mainEra $ ctx
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment",  amt <> "@" <> addr
                ]

        (c, out, err) <- postTransactionViaCLI ctx "This password is wrong" args
        (T.unpack err) `shouldContain` errMsg403WrongPass
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    describe "TRANS_CREATE_05 - Invalid addresses" $ do
        forM_ matrixInvalidAddrs $ \(title, addr, errMsg) -> it title $ \ctx -> runResourceT $ do
            wSrc <- emptyWallet ctx
            let args = T.unpack <$>
                    [ wSrc ^. walletId
                    , "--payment", "12@" <> (T.pack addr)
                    ]

            (c, out, err) <- postTransactionViaCLI ctx
                (T.unpack fixturePassphrase) args
            (T.unpack err) `shouldContain` errMsg
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1

    describe "TRANS_CREATE_06 - Invalid amount" $ do
        forM_ matrixInvalidAmt $ \(title, amt, errMsg) -> it title $ \ctx -> runResourceT $ do
            wSrc <- emptyWallet ctx
            wDest <- emptyWallet ctx
            addrs:_ <- listAddresses @n ctx wDest
            let addr = encodeAddress (sNetworkId @n) (apiAddress $ addrs ^. #id)
            let args = T.unpack <$>
                    [ wSrc ^. walletId
                    , "--payment", amt <> "@" <> addr
                    ]

            (c, out, err) <- postTransactionViaCLI ctx
                (T.unpack fixturePassphrase) args
            (T.unpack err) `shouldContain` errMsg
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1

    describe "TRANS_CREATE_07 - False wallet ids" $ do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> runResourceT $ do
            wDest <- emptyWallet ctx
            addrs:_ <- listAddresses @n ctx wDest
            let port = show $ ctx ^. typed @(Port "wallet")
            let addr = encodeAddress (sNetworkId @n) (apiAddress $ addrs ^. #id)
            let args =
                    [ "transaction", "create", "--port", port
                    , walId, "--payment", "12@" ++  (T.unpack addr)
                    ]
            -- make sure CLI returns error before asking for passphrase
            (Exit c, Stdout out, Stderr err) <- cardanoWalletCLI args
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1
            if (title == "40 chars hex") then
                err `shouldContain` "I couldn't find a wallet with \
                    \the given id: 1111111111111111111111111111111111111111"
            else
                err `shouldContain` "wallet id should be a \
                    \hex-encoded string of 40 characters"

    it "TRANS_CREATE_07 - 'almost' valid walletId" $ \ctx -> runResourceT $ do
        wSrc <- emptyWallet ctx
        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses @n ctx wDest
        let port = T.pack $ show $ ctx ^. typed @(Port "wallet")
        let addr = encodeAddress (sNetworkId @n) (apiAddress $ addrs ^. #id)
        let amt = T.pack . show . minUTxOValue . _mainEra $ ctx
        let args = T.unpack <$>
                [ "transaction", "create", "--port", port
                , T.append (wSrc ^. walletId) "0", "--payment", amt <> "@" <> addr
                ]
        -- make sure CLI returns error before asking for passphrase
        (Exit c, Stdout out, Stderr err) <- cardanoWalletCLI args
        err `shouldContain` "wallet id should be a hex-encoded\
            \ string of 40 characters"
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    it "TRANS_CREATE_07 - Deleted wallet" $ \ctx -> runResourceT $ do
        wSrc <- emptyWallet ctx
        Exit ex <- deleteWalletViaCLI ctx (T.unpack ( wSrc ^. walletId ))
        ex `shouldBe` ExitSuccess

        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses @n ctx wDest
        let addr = encodeAddress (sNetworkId @n) (apiAddress $ addrs ^. #id)
        let port = T.pack $ show $ ctx ^. typed @(Port "wallet")
        let amt = T.pack . show . minUTxOValue . _mainEra $ ctx
        let args = T.unpack <$>
                [ "transaction", "create", "--port", port
                , wSrc ^. walletId, "--payment", amt <> "@" <> addr
                ]
        -- make sure CLI returns error before asking for passphrase
        (Exit c, Stdout out, Stderr err) <- cardanoWalletCLI args
        err `shouldContain` "I couldn't find a wallet with \
            \the given id: " ++ T.unpack ( wSrc ^. walletId )
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    it "TRANSMETA_CREATE_01a - \
        \Transaction with metadata via CLI" $
        \ctx -> runResourceT $ do

        (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
        let amt = 10_000_000
        let md = Just "{ \"1\": { \"string\": \"hello\" } }"
        let expected =
                Just $
                detailedMetadata $
                TxMetadata $
                    Map.singleton 1 (TxMetaText "hello")

        args <- postTxArgs ctx wSrc wDest amt md Nothing
        Stdout feeOut <- postTransactionFeeViaCLI ctx args
        ApiFee (Quantity feeMin) (Quantity feeMax) _ _ <- expectValidJSON Proxy feeOut

        txJson <- postTxViaCLI ctx wSrc wDest amt md Nothing
        verify txJson
            [ expectCliField
                (#amount . #getQuantity)
                (between (feeMin + amt, feeMax + amt))
            , expectCliField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectCliField (#status . #getApiT) (`shouldBe` Pending)
            , expectCliField #metadata (`shouldBe` expected)
            ]

        eventually "metadata is confirmed in transaction list" $ do
            (Exit code, Stdout out, Stderr err) <-
                listTransactionsViaCLI ctx TxMetadataDetailedSchema
                    [T.unpack $ wSrc ^. walletId]
            err `shouldBe` "Ok.\n"
            code `shouldBe` ExitSuccess
            outJson <- expectValidJSON (Proxy @([ApiTransaction n])) out
            verify
                outJson
                [ expectCliListField 0 #metadata (`shouldBe` expected)
                , expectCliListField 0
                    (#status . #getApiT)
                    (`shouldBe` InLedger)
                ]

    it "TRANSMETA_CREATE_01b - \
        \Transaction with metadata via CLI with simple metadata" $ \ctx ->
        runResourceT $ do

        -- Prepare test fixture:
        srcWallet <- fixtureWallet ctx
        let srcWalletId = T.unpack (srcWallet ^. walletId)
        dstWallet <- emptyWallet ctx
        let amount = 10_000_000
        let metadata = Just "{ \"1\": { \"string\": \"hello\" } }"
        let txMetadata = TxMetadata $ Map.singleton 1 (TxMetaText "hello")
        let successfulJsonCliResponse proxy f = do
                (Exit code, Stdout out, Stderr err) <- f
                err `shouldBe` "Ok.\n"
                code `shouldBe` ExitSuccess
                expectValidJSON proxy out

        -- Post transaction with metadata using CLI:
        postedTx <-
            postTxViaCLI ctx srcWallet dstWallet amount metadata Nothing
        verify postedTx
            [ expectCliField #metadata
                (`shouldBe` Just (detailedMetadata txMetadata)) ]

        -- Query posted transaction by its id using CLI:
        queriedTx <- successfulJsonCliResponse (Proxy @(ApiTransaction n)) $
            getTransactionViaCLI ctx srcWalletId (getTxId postedTx)
                TxMetadataNoSchema

        -- Verify that correct metadata is present in the query result:
        verify queriedTx
            [ expectCliField #metadata
                (`shouldBe` Just (noSchemaMetadata txMetadata)) ]

        eventually "metadata is confirmed in transaction list" $ do
            txList <-
                successfulJsonCliResponse (Proxy @([ApiTransaction n])) $
                listTransactionsViaCLI ctx TxMetadataNoSchema [srcWalletId]
            verify txList
                [ expectCliListField 0 #metadata
                    (`shouldBe` Just (noSchemaMetadata txMetadata))
                , expectCliListField 0 (#status . #getApiT)
                    (`shouldBe` InLedger)
                ]

    it "TRANSTTL_CREATE_01 - Transaction with TTL via CLI" $ \ctx -> runResourceT $ do
      (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
      let amt = 10_000_000
      let ttl = Just "30s"

      args <- postTxArgs ctx wSrc wDest amt Nothing ttl
      Stdout feeOut <- postTransactionFeeViaCLI ctx args
      ApiFee (Quantity feeMin) (Quantity feeMax) _ _ <- expectValidJSON Proxy feeOut

      txJson <- postTxViaCLI ctx wSrc wDest amt Nothing ttl
      verify txJson
          [ expectCliField (#amount . #getQuantity)
              (between (feeMin + amt, feeMax + amt))
          , expectCliField (#direction . #getApiT) (`shouldBe` Outgoing)
          , expectCliField (#status . #getApiT) (`shouldBe` Pending)
          ]

      eventually "transaction with ttl is confirmed in transaction list" $ do
          (Exit code, Stdout out, Stderr err) <-
              listTransactionsViaCLI ctx TxMetadataDetailedSchema
                  [T.unpack $ wDest ^. walletId]
          err `shouldBe` "Ok.\n"
          code `shouldBe` ExitSuccess
          outJson <- expectValidJSON (Proxy @([ApiTransaction n])) out
          verify outJson
              [ expectCliListField 0 (#status . #getApiT) (`shouldBe` InLedger) ]

    describe "TRANS_ESTIMATE_08 - Invalid addresses" $ do
        forM_ matrixInvalidAddrs $ \(title, addr, errMsg) -> it title $ \ctx -> runResourceT $ do
            wSrc <- emptyWallet ctx
            let amt = T.pack . show . minUTxOValue . _mainEra $ ctx
            let args = T.unpack <$>
                    [ wSrc ^. walletId
                    , "--payment", amt <> "@" <> (T.pack addr)
                    ]

            (Exit c, Stdout out, Stderr err) <- postTransactionFeeViaCLI ctx args
            err `shouldContain` errMsg
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1

    describe "TRANS_ESTIMATE_09 - Invalid amount" $ do
        forM_ matrixInvalidAmt $ \(title, amt, errMsg) -> it title $ \ctx -> runResourceT $ do
            wSrc <- emptyWallet ctx
            wDest <- emptyWallet ctx
            addrs:_ <- listAddresses @n ctx wDest
            let addr = encodeAddress (sNetworkId @n) (apiAddress $ addrs ^. #id)
            let args = T.unpack <$>
                    [ wSrc ^. walletId
                    , "--payment", amt <> "@" <> addr
                    ]

            (c, out, err) <- postTransactionViaCLI ctx "cardano-wallet" args
            (T.unpack err) `shouldContain` errMsg
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1

    describe "TRANS_LIST_01 - Listing transactions for an empty wallet" $
        forM_ timeRangeMatrix $ \(mStart, mEnd) -> do
            forM_ sortOrderMatrix $ \mOrder -> do
                let title = mempty
                      <> "listing transactions from "
                      <> show mStart
                      <> " to "
                      <> show mEnd
                      <> " in "
                      <> show mOrder
                      <> " order "
                it title $ \ctx -> runResourceT $ do
                    wallet <- emptyWallet ctx
                    (Exit code, Stdout out, Stderr err) <-
                        listTransactionsViaCLI ctx TxMetadataDetailedSchema $
                            join
                                [ [T.unpack $ wallet ^. walletId]
                                , maybe [] (\t -> ["--start", t]) mStart
                                , maybe [] (\t -> ["--end"  , t]) mEnd
                                , maybe [] (\o -> ["--order", showT o]) mOrder
                                ]
                    err `shouldBe` "Ok.\n"
                    out `shouldBe` "[]\n"
                    code `shouldBe` ExitSuccess

    it "TRANS_LIST_01 - Can list Incoming and Outgoing transactions" $ \ctx -> runResourceT $ do
        -- Make tx from fixtureWallet
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses @n ctx wDest
        let addrStr = encodeAddress (sNetworkId @n) (apiAddress $ addr ^. #id)
        let amt = minUTxOValue (_mainEra ctx) :: Natural
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addrStr
                ]

        -- post transaction
        (c, _, _) <- postTransactionViaCLI ctx "cardano-wallet" args
        c `shouldBe` ExitSuccess
        eventually "Balance on wallet is as expected" $ do
            Stdout gOutDest <- getWalletViaCLI ctx
                (T.unpack (wDest ^. walletId))
            destJson <- expectValidJSON (Proxy @ApiWallet) gOutDest
            verify destJson
                [ expectCliField
                        (#balance . #available) (`shouldBe` Quantity amt)
                , expectCliField
                        (#balance . #total) (`shouldBe` Quantity amt)
                ]

        -- Verify Tx list contains Incoming and Outgoing
        (Exit code, Stdout out, Stderr err) <-
            listTransactionsViaCLI ctx TxMetadataDetailedSchema
                [T.unpack $ wSrc ^. walletId]
        err `shouldBe` "Ok.\n"
        code `shouldBe` ExitSuccess
        outJson <- expectValidJSON (Proxy @([ApiTransaction n])) out
        verify outJson
            [ expectCliListField 0 (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectCliListField 1 (#direction . #getApiT) (`shouldBe` Incoming)
            ]

    describe "TRANS_LIST_02 - Start time shouldn't be later than end time" $
        forM_ sortOrderMatrix $ \mOrder -> do
            let startTime = max validTime1 validTime2
            let endTime   = min validTime1 validTime2
            let title = mempty
                    <> "listing transactions from "
                    <> show startTime
                    <> " to "
                    <> show endTime
                    <> " in "
                    <> show mOrder
                    <> " order "
            it title $ \ctx -> runResourceT $ do
                wid <- emptyWallet' ctx
                (Exit code, Stdout out, Stderr err) <-
                    listTransactionsViaCLI ctx TxMetadataDetailedSchema $ join
                        [ [ wid ]
                        , [ "--start", startTime ]
                        , [ "--end"  , endTime ]
                        , maybe [] (\o -> ["--order", showT o]) mOrder
                        ]
                err `shouldBe` mconcat
                    [ "The specified start time '"
                    , startTime
                    , "' is later than the specified end time '"
                    , endTime
                    , "'.\n"
                    ]
                out `shouldBe` mempty
                code `shouldBe` ExitFailure 1

    it "TRANS_LIST_03 - Can order results" $ \ctx -> runResourceT $ do
        let a1 = Quantity $ sum $ replicate 10 (minUTxOValue (_mainEra ctx))
        let a2 = Quantity $ sum $ replicate 10 (2 * (minUTxOValue (_mainEra ctx)))
        w <- fixtureWalletWith @n ctx $ mconcat
                [ replicate 10 (minUTxOValue (_mainEra ctx))
                , replicate 10 (2 * (minUTxOValue (_mainEra ctx)))
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
                listTransactionsViaCLI ctx TxMetadataDetailedSchema args
            err `shouldBe` "Ok.\n"
            code `shouldBe` ExitSuccess
            outJson <- expectValidJSON (Proxy @([ApiTransaction n])) out
            length outJson `shouldBe` 2
            verify outJson expects

    describe "TRANS_LIST_02,03 - Faulty start, end, order values" $ do
        let orderErr = "Please specify one of the following values:\
            \ ascending, descending."
        let startEndErr = "Expecting ISO 8601 date-and-time format\
            \ (basic or extended), e.g. 2012-09-25T10:15:00Z."
        let queries =
                  [ ( [ "--start", "2009" ]
                    , startEndErr
                    )
                  ,
                    ( [ "--start", "2012-09-25T10:15:00Z", "--end", "2016-11-21" ]
                    , startEndErr
                    )
                  ,
                    ( [ "--start", "2012-09-25", "--end", "2016-11-21T10:15:00Z" ]
                    , startEndErr
                    )
                  ,
                    ( [ "--end", "2012-09-25T10:15:00Z", "--start", "2016-11-21" ]
                    , startEndErr
                    )
                  ,
                    ( [ "--order", "scending" ]
                    , orderErr
                    )
                  ,
                    ( [ "--start", "2012-09-25T10:15:00Z", "--order", "asc" ]
                    , orderErr
                    )
                  ]
        forM_ queries $ \(q, errorMess) -> it (unwords q) $ \ctx -> runResourceT $ do
            wid <- emptyWallet' ctx
            let args = wid : q
            (Exit code, Stdout out, Stderr err) <-
                listTransactionsViaCLI ctx TxMetadataDetailedSchema args
            out `shouldBe` mempty
            code `shouldBe` ExitFailure 1
            err `shouldContain` errorMess

    it "TRANS_LIST_04 - 'almost' valid walletId" $ \ctx -> runResourceT $ do
        wid <- emptyWallet' ctx
        let invalidWid = wid ++ "0"
        (Exit code, Stdout out, Stderr err) <-
            listTransactionsViaCLI ctx TxMetadataDetailedSchema [invalidWid]

        err `shouldContain` "wallet id should be a hex-encoded\
            \ string of 40 characters"
        code `shouldBe` ExitFailure 1
        out `shouldBe` mempty

    it "TRANS_LIST_04 - Deleted wallet" $ \ctx -> runResourceT $ do
        wid <- emptyWallet' ctx
        Exit d <- deleteWalletViaCLI ctx wid
        d `shouldBe` ExitSuccess

        (Exit c, Stdout o, Stderr e) <-
            listTransactionsViaCLI ctx TxMetadataDetailedSchema [wid]
        e `shouldContain` errMsg404NoWallet (T.pack wid)
        o `shouldBe` mempty
        c `shouldBe` ExitFailure 1

    describe "TRANS_LIST_04 - False wallet ids" $ do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> runResourceT $ do
            (Exit c, Stdout o, Stderr e) <-
                listTransactionsViaCLI ctx TxMetadataDetailedSchema [walId]
            o `shouldBe` ""
            c `shouldBe` ExitFailure 1
            if (title == "40 chars hex") then
                e `shouldContain`
                    errMsg404NoWallet "1111111111111111111111111111111111111111"
            else
                e `shouldContain`
                    "wallet id should be a hex-encoded string of 40 characters"

    it "TRANS_LIST_RANGE_01 - \
       \Transaction at time t is SELECTED by small ranges that cover it" $
          \ctx -> runResourceT $ do
              w <- fixtureWalletWith @n ctx [(minUTxOValue (_mainEra ctx))]
              let walId = w ^. walletId
              t <- unsafeGetTransactionTime =<< listAllTransactions @n ctx w
              let (te, tl) = (utcTimePred t, utcTimeSucc t)
              let query t1 t2 =
                        [ "--start", utcIso8601ToText t1
                        , "--end", utcIso8601ToText t2
                        ]
              Stdout o1 <- listTransactionsViaCLI ctx TxMetadataDetailedSchema
                  (T.unpack <$> walId : (query t t))
              Stdout o2 <- listTransactionsViaCLI ctx TxMetadataDetailedSchema
                  (T.unpack <$> walId : (query te t))
              Stdout o3 <- listTransactionsViaCLI ctx TxMetadataDetailedSchema
                  (T.unpack <$> walId : (query t tl))
              Stdout o4 <- listTransactionsViaCLI ctx TxMetadataDetailedSchema
                  (T.unpack <$> walId : (query te tl))
              oJson1 <- expectValidJSON (Proxy @([ApiTransaction n])) o1
              oJson2 <- expectValidJSON (Proxy @([ApiTransaction n])) o2
              oJson3 <- expectValidJSON (Proxy @([ApiTransaction n])) o3
              oJson4 <- expectValidJSON (Proxy @([ApiTransaction n])) o4
              length <$> [oJson1, oJson2, oJson3, oJson4] `shouldSatisfy` all (== 1)

    it "TRANS_LIST_RANGE_02 - \
       \Transaction at time t is NOT selected by range [t + 𝛿t, ...)" $
          \ctx -> runResourceT $ do
              w <- fixtureWalletWith @n ctx [(minUTxOValue (_mainEra ctx))]
              let walId = w ^. walletId
              t <- unsafeGetTransactionTime =<< listAllTransactions @n ctx w
              let tl = utcIso8601ToText $ utcTimeSucc t
              Stdout o1 <- listTransactionsViaCLI ctx TxMetadataDetailedSchema
                  (T.unpack <$> [walId, "--start", tl])
              Stdout o2 <- listTransactionsViaCLI ctx TxMetadataDetailedSchema
                  (T.unpack <$> [walId, "--start", tl, "--end", tl])
              oJson1 <- expectValidJSON (Proxy @([ApiTransaction n])) o1
              oJson2 <- expectValidJSON (Proxy @([ApiTransaction n])) o2
              length <$> [oJson1, oJson2] `shouldSatisfy` all (== 0)

    it "TRANS_LIST_RANGE_03 - \
       \Transaction at time t is NOT selected by range (..., t - 𝛿t]" $
          \ctx -> runResourceT $ do
              w <- fixtureWalletWith @n ctx [(minUTxOValue (_mainEra ctx))]
              let walId = w ^. walletId
              t <- unsafeGetTransactionTime =<< listAllTransactions @n ctx w
              let te = utcIso8601ToText $ utcTimePred t
              Stdout o1 <- listTransactionsViaCLI ctx TxMetadataDetailedSchema
                  (T.unpack <$> [walId, "--end", te])
              Stdout o2 <- listTransactionsViaCLI ctx TxMetadataDetailedSchema
                  (T.unpack <$> [walId, "--start", te, "--end", te])
              oJson1 <- expectValidJSON (Proxy @([ApiTransaction n])) o1
              oJson2 <- expectValidJSON (Proxy @([ApiTransaction n])) o2
              length <$> [oJson1, oJson2] `shouldSatisfy` all (== 0)

    it "TRANS_GET_01 - Can get Incoming and Outgoing transaction" $ \ctx -> runResourceT $ do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses @n ctx wDest
        let addrStr = encodeAddress (sNetworkId @n) (apiAddress $ addr ^. #id)
        let amt = (minUTxOValue (_mainEra ctx)) :: Natural
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addrStr
                ]
        (c, out, err) <- postTransactionViaCLI ctx "cardano-wallet" args
        err `shouldBe` "Please enter your passphrase: **************\nOk.\n"
        txJson <- expectValidJSON (Proxy @(ApiTransaction n)) out
        verify txJson
            [ expectCliField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectCliField (#status . #getApiT) (`shouldBe` Pending)
            ]
        c `shouldBe` ExitSuccess

        eventually "Balance on wallet is as expected" $ do
            Stdout gOutDest <- getWalletViaCLI ctx
                (T.unpack (wDest ^. walletId))
            destJson <- expectValidJSON (Proxy @ApiWallet) gOutDest
            verify destJson
                [ expectCliField
                        (#balance . #available) (`shouldBe` Quantity amt)
                , expectCliField
                        (#balance . #total) (`shouldBe` Quantity amt)
                ]

        eventually "Transactions are available and in ledger" $ do
            let wSrcId = T.unpack (wSrc ^. walletId)
            let txId =  getTxId txJson

            -- Verify Tx in source wallet is Outgoing and InLedger
            (Exit code1, Stdout out1, Stderr err1) <-
                getTransactionViaCLI ctx wSrcId txId TxMetadataDetailedSchema
            err1 `shouldBe` "Ok.\n"
            code1 `shouldBe` ExitSuccess
            outJson1 <- expectValidJSON (Proxy @(ApiTransaction n)) out1
            verify outJson1
                [ expectCliField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectCliField (#status . #getApiT) (`shouldBe` InLedger)
                ]

            let wDestId = T.unpack (wDest ^. walletId)
            -- Verify Tx in destination wallet is Incoming and InLedger
            (Exit code2, Stdout out2, Stderr err2) <-
                getTransactionViaCLI ctx wDestId txId TxMetadataDetailedSchema
            err2 `shouldBe` "Ok.\n"
            code2 `shouldBe` ExitSuccess
            outJson2 <- expectValidJSON (Proxy @(ApiTransaction n)) out2
            verify outJson2
                [ expectCliField (#direction . #getApiT) (`shouldBe` Incoming)
                , expectCliField (#status . #getApiT) (`shouldBe` InLedger)
                ]

    it "TRANS_GET_02 - Deleted wallet" $ \ctx -> runResourceT $ do
        wid <- emptyWallet' ctx
        Exit d <- deleteWalletViaCLI ctx wid
        d `shouldBe` ExitSuccess
        let txId = "3e6ec12da4414aa0781ff8afa9717ae53ee8cb4aa55d622f65bc62619a4f7b12"

        (Exit c, Stdout o, Stderr e) <-
            getTransactionViaCLI ctx wid txId TxMetadataDetailedSchema
        e `shouldContain` errMsg404NoWallet (T.pack wid)
        o `shouldBe` mempty
        c `shouldBe` ExitFailure 1

    it "TRANS_GET_03 - Using wrong transaction id" $ \ctx -> runResourceT $ do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses @n ctx wDest
        let addrStr = encodeAddress (sNetworkId @n) (apiAddress $ addr ^. #id)
        let amt = (minUTxOValue (_mainEra ctx)) :: Natural
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addrStr
                ]
        (c1, o1, e1) <- postTransactionViaCLI ctx "cardano-wallet" args
        e1 `shouldBe` "Please enter your passphrase: **************\nOk.\n"
        txJson <- expectValidJSON (Proxy @(ApiTransaction n)) o1
        verify txJson
            [ expectCliField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectCliField (#status . #getApiT) (`shouldBe` Pending)
            ]
        c1 `shouldBe` ExitSuccess

        let wid = T.unpack (wSrc ^. walletId)
        let txId = "3e6ec12da4414aa0781ff8afa9717ae53ee8cb4aa55d622f65bc62619a4f7b12"
        (Exit c2, Stdout o2, Stderr e2) <-
            getTransactionViaCLI ctx wid txId TxMetadataDetailedSchema
        e2 `shouldContain` errMsg404CannotFindTx (T.pack txId)
        o2 `shouldBe` mempty
        c2 `shouldBe` ExitFailure 1


    it "TRANS_DELETE_01 - Cannot forget pending transaction when not pending anymore via CLI" $ \ctx -> runResourceT $ do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        let wSrcId = T.unpack (wSrc ^. walletId)

        -- post transaction
        txJson <- postTxViaCLI ctx wSrc wDest (minUTxOValue (_mainEra ctx)) Nothing Nothing
        verify txJson
            [ expectCliField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectCliField (#status . #getApiT) (`shouldBe` Pending)
            ]
        let txId =  getTxId txJson

        eventually "Tx is in ledger" $ do
            listTransactionsViaCLI ctx TxMetadataDetailedSchema [wSrcId]
                >>= expectValidJSON (Proxy @([ApiTransaction n])) . fromStdout
                >>= flip verify
                    [ expectCliListField 0
                        (#direction . #getApiT) (`shouldBe` Outgoing)
                    , expectCliListField 0
                        (#status . #getApiT) (`shouldBe` InLedger)
                    ]

     -- Try Forget transaction once it's no longer pending
        (Exit c2, Stdout out2, Stderr err2) <-
            deleteTransactionViaCLI ctx wSrcId txId
        err2 `shouldContain` errMsg403AlreadyInLedger (T.pack txId)
        out2 `shouldBe` ""
        c2 `shouldBe` ExitFailure 1

    it "TRANS_DELETE_03 - Cannot forget tx that is not found via CLI" $ \ctx -> runResourceT $ do
        wid <- fixtureWallet' ctx
        let txId = "3e6ec12da4414aa0781ff8afa9717ae53ee8cb4aa55d622f65bc62619a4f7b12"
        -- forget transaction
        (Exit c, Stdout out, Stderr err) <-
            deleteTransactionViaCLI ctx wid (T.unpack txId)
        err `shouldContain` errMsg404CannotFindTx txId
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    describe "TRANS_DELETE_04 - False wallet ids via CLI" $ do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> runResourceT $ do
            let txId = "3e6ec12da4414aa0781ff8afa9717ae53ee8cb4aa55d622f65bc62619a4f7b12"
            -- forget transaction once again
            (Exit c, Stdout out, Stderr err) <-
                deleteTransactionViaCLI ctx walId (T.unpack txId)
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
        $ \ctx -> runResourceT $ do
            -- post tx
            wSrc <- fixtureWallet ctx
            wDest <- emptyWallet ctx
            txJson <- postTxViaCLI ctx wSrc wDest (minUTxOValue (_mainEra ctx)) Nothing Nothing

            -- try to forget from different wallet
            widDiff <- emptyWallet' ctx
            let txId = getTxId txJson
            (Exit c, Stdout out, Stderr err) <-
                deleteTransactionViaCLI ctx widDiff txId
            err `shouldContain` errMsg404CannotFindTx (T.pack txId)
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1

    describe "TRANS_DELETE_07 - invalid tx id via CLI" $ do
        let txIds =
                [ replicate 63 '1'
                , replicate 65 '1'
                , replicate 64 'ś'
                ]
        forM_ txIds $ \tid -> it (show tid) $ \ctx -> runResourceT $ do
            wid <- emptyWallet' ctx
            (Exit c, Stdout out, Stderr err) <-
                deleteTransactionViaCLI ctx wid tid
            err `shouldContain`
                "should be a hex-encoded string of 64 characters"
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1

    it "BYRON_TX_LIST_03 -\
        \ Shelley CLI does not list Byron wallet transactions" $ \ctx -> runResourceT $ do
        wid <- emptyRandomWallet' ctx
        (Exit c, Stdout o, Stderr e) <-
            listTransactionsViaCLI ctx TxMetadataDetailedSchema [wid]
        e `shouldContain` errMsg404NoWallet (T.pack wid)
        o `shouldBe` mempty
        c `shouldBe` ExitFailure 1

    it "BYRON_TRANS_DELETE -\
        \ Cannot delete tx on Byron wallet using shelley CLI" $ \ctx -> runResourceT $ do
        wid <- emptyRandomWallet' ctx
        (Exit c, Stdout o, Stderr e)
            <- deleteTransactionViaCLI ctx wid (replicate 64 '1')
        e `shouldContain` errMsg404NoWallet (T.pack wid)
        o `shouldBe` mempty
        c `shouldBe` ExitFailure 1

    describe "BYRON_TRANS_CREATE / BYRON_TRANS_ESTIMATE -\
        \ Cannot create/estimate tx on Byron wallet using shelley CLI" $ do
        forM_ ["create", "fees"] $ \action -> it action $ \ctx -> runResourceT $ do
            wSrc <- emptyRandomWallet ctx
            wDest <- emptyWallet ctx
            addrs:_ <- listAddresses @n ctx wDest
            let addr = encodeAddress (sNetworkId @n) (apiAddress $ addrs ^. #id)
            let port = T.pack $ show $ ctx ^. typed @(Port "wallet")
            let args = T.unpack <$>
                    [ "transaction", T.pack action, "--port", port
                    , wSrc ^. walletId, "--payment", T.pack (show . minUTxOValue . _mainEra $ ctx) <> "@" <> addr
                    ]
            -- make sure CLI returns error before asking for passphrase
            (Exit c, Stdout out, Stderr err) <- cardanoWalletCLI args
            err `shouldContain` "I couldn't find a wallet with \
                \the given id: " ++ T.unpack ( wSrc ^. walletId )
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1
  where
      postTxViaCLI
          :: MonadUnliftIO m
          => Context
          -> ApiWallet
          -> ApiWallet
          -> Natural
          -> Maybe Text
          -> Maybe Text
          -> m (ApiTransaction n)
      postTxViaCLI ctx wSrc wDest amt md ttl = do
          args <- postTxArgs ctx wSrc wDest amt md ttl

          -- post transaction
          (c, out, err) <- postTransactionViaCLI ctx "cardano-wallet" args
          err `shouldBe` "Please enter your passphrase: **************\nOk.\n"
          c `shouldBe` ExitSuccess
          expectValidJSON (Proxy @(ApiTransaction n)) out

      postTxArgs
        :: MonadUnliftIO m
        => Context
        -> ApiWallet
        -> ApiWallet
        -> Natural
        -> Maybe Text
        -> Maybe Text
        -> m [String]
      postTxArgs ctx wSrc wDest amt md ttl = do
          addr <- headMayIO =<< listAddresses @n ctx wDest
          let addrStr = encodeAddress (sNetworkId @n) (apiAddress $ addr ^. #id)
          return $ T.unpack <$>
              [ wSrc ^. walletId
              , "--payment", T.pack (show amt) <> "@" <> addrStr
              ] ++ maybe [] (\json -> ["--metadata", json]) md
              ++ maybe [] (\s -> ["--ttl", s]) ttl

      fixtureWallet' :: Context -> ResourceT IO String
      fixtureWallet' = fmap (T.unpack . view walletId) . fixtureWallet

      emptyWallet' :: Context -> ResourceT IO String
      emptyWallet' = fmap (T.unpack . view walletId) . emptyWallet

      emptyRandomWallet' :: Context -> ResourceT IO String
      emptyRandomWallet' = fmap (T.unpack . view walletId) . emptyRandomWallet

      sortOrderMatrix :: [Maybe SortOrder]
      sortOrderMatrix = Nothing : fmap pure enumerate

      validTime1 = "2001-01-01T01:01:01Z"
      validTime2 = "2009-09-09T09:09:09Z"

      timeRangeMatrix :: [(Maybe String, Maybe String)]
      timeRangeMatrix =
          [ (Nothing, Nothing)
          , (Just validTime1, Nothing)
          , (Nothing, Just validTime2)
          , (Just validTime1, Just validTime2)
          ]

      encodeErr = "Unrecognized address encoding"
      parseErr = "Parse error. Expecting format \"<amount>@<address>\""
      matrixInvalidAddrs =
          [ ( "-1000", "-1000", encodeErr )
          , ( "q", "q", encodeErr )
          , ( "empty", "", encodeErr )
          , ( "wildcards", T.unpack wildcardsWalletName, parseErr )
          , ( "arabic", T.unpack arabicWalletName, encodeErr )
          , ( "kanji", T.unpack kanjiWalletName, encodeErr )
          , ( "polish", T.unpack polishWalletName, encodeErr )
          , ( "[]", "[]", encodeErr )
          , ( "no address", "", encodeErr )
          , ( "address is space", " ", encodeErr )
          , ( "invalid Byron", T.unpack invalidByronBase58, encodeErr)
          ]
      errNum = "Expecting natural number"
      matrixInvalidAmt =
          [ ("1.5", "1.5", errNum)
          , ("-1000", "-1000", errNum)
          , ("[]", "[]", errNum)
          , ("string with diacritics", polishWalletName, errNum)
          , ("string with wildcards", wildcardsWalletName, parseErr)
          , ("no amount", "", errNum)
          ]

headMayIO :: (HasCallStack, MonadIO m) => [a] -> m a
headMayIO [] = throwString "List was empty, but expected non-empty"
headMayIO (x:_) = pure x
