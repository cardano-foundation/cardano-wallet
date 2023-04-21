{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- DBLayer tests for SQLite implementation.
--
-- To test individual properties in GHCi, use the shorthand type aliases. For
-- example:
--
-- >>> db <- newMemoryDBLayer :: IO TestDBSeq
-- >>> quickCheck $ prop_sequential db

module Cardano.Wallet.DB.LayerSpec
    ( spec
    ) where

import Prelude

import Cardano.BM.Configuration.Static
    ( defaultConfigTesting )
import Cardano.BM.Data.Tracer
    ( nullTracer )
import Cardano.BM.Setup
    ( setupTrace )
import Cardano.BM.Trace
    ( traceInTVarIO )
import Cardano.Crypto.Wallet
    ( XPrv )
import Cardano.DB.Sqlite
    ( DBField, DBLog (..), fieldName )
import Cardano.Mnemonic
    ( SomeMnemonic (..) )
import Cardano.Wallet
    ( mkNoSuchWalletError )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , Index
    , PaymentAddress (..)
    , PersistPrivateKey
    , WalletKey
    )
import Cardano.Wallet.Address.Derivation.Byron
    ( ByronKey (..) )
import Cardano.Wallet.Address.Derivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Address.Derivation.Shared
    ()
import Cardano.Wallet.Address.Derivation.SharedKey
    ( SharedKey )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey (..), generateKeyFromSeed )
import Cardano.Wallet.DB
    ( DBFactory (..), DBLayer (..) )
import Cardano.Wallet.DB.Layer
    ( DefaultFieldValues (..)
    , PersistAddressBook
    , WalletDBLog (..)
    , newDBFactory
    , newDBLayerInMemory
    , retrieveWalletId
    , withDBLayer
    , withDBLayerFromDBOpen
    , withDBLayerInMemory
    , withDBOpenFromFile
    )
import Cardano.Wallet.DB.Properties
    ( properties )
import Cardano.Wallet.DB.Sqlite.Migration
    ( InvalidDatabaseSchemaVersion (..)
    , SchemaVersion (..)
    , currentSchemaVersion
    )
import Cardano.Wallet.DB.StateMachine
    ( TestConstraints, prop_sequential, validateGenerators )
import Cardano.Wallet.DB.WalletState
    ( ErrNoSuchWallet (..) )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( block0, dummyGenesisParameters, dummyTimeInterpreter )
import Cardano.Wallet.Gen
    ( genMnemonic )
import Cardano.Wallet.Logging
    ( trMessageText )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( KnownAddresses (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( DerivationPrefix (..)
    , SeqState (..)
    , coinTypeAda
    , defaultAddressPoolGap
    , mkSeqStateFromRootXPrv
    , purposeBIP44
    , purposeCIP1852
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( SharedState )
import Cardano.Wallet.Primitive.Model
    ( FilteredBlock (..)
    , Wallet
    , applyBlock
    , availableBalance
    , currentTip
    , getState
    , initWallet
    )
import Cardano.Wallet.Primitive.Passphrase
    ( encryptPassphrase, preparePassphrase )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..)
    , PassphraseHash (..)
    , PassphraseScheme (..)
    , WalletPassphraseInfo (..)
    )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , Block (..)
    , BlockHeader (..)
    , GenesisParameters (..)
    , Range
    , SlotNo (..)
    , SortOrder (..)
    , StartTime (..)
    , WalletDelegation (..)
    , WalletDelegationStatus (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WithOrigin (At)
    , wholeRange
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..), mockHash )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..)
    , TransactionInfo (..)
    , Tx (..)
    , TxMeta (..)
    , TxScriptValidity (..)
    , TxStatus (..)
    , toTxHistory
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Read.NetworkId
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex, unsafeRunExceptT )
import Control.Monad
    ( forM_, forever, replicateM_, unless, void )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, mapExceptT )
import Control.Tracer
    ( Tracer )
import Crypto.Hash
    ( hash )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Generics.Internal.VL.Lens
    ( over, view, (^.) )
import Data.Generics.Labels
    ()
import Data.Maybe
    ( fromMaybe, isJust, isNothing, mapMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.String.Interpolate
    ( i )
import Data.Text
    ( Text )
import Data.Text.Class
    ( toText )
import Data.Time.Clock
    ( getCurrentTime )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Data.Typeable
    ( Typeable, typeOf )
import Data.Word
    ( Word64 )
import Database.Persist.Sql
    ( FieldNameDB (..), PersistEntity (..), fieldDB )
import Database.Persist.Sqlite
    ( Single (..) )
import Numeric.Natural
    ( Natural )
import Safe
    ( headMay )
import System.Directory
    ( copyFile, doesFileExist, listDirectory, removeFile )
import System.FilePath
    ( (</>) )
import System.IO
    ( IOMode (..), hClose, withFile )
import System.IO.Error
    ( isUserError )
import System.IO.Temp
    ( emptySystemTempFile )
import System.IO.Unsafe
    ( unsafePerformIO )
import System.Random
    ( randomRIO )
import Test.Hspec
    ( Expectation
    , Spec
    , SpecWith
    , around
    , before
    , beforeWith
    , describe
    , it
    , shouldBe
    , shouldNotBe
    , shouldNotContain
    , shouldReturn
    , shouldSatisfy
    , shouldThrow
    )
import Test.QuickCheck
    ( NonEmptyList (..), Property, generate, property, (==>) )
import Test.QuickCheck.Monadic
    ( monadicIO )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.Trace
    ( captureLogging )
import UnliftIO.Async
    ( concurrently, concurrently_ )
import UnliftIO.Concurrent
    ( forkIO, killThread, threadDelay )
import UnliftIO.Exception
    ( SomeException, handle, throwIO )
import UnliftIO.MVar
    ( isEmptyMVar, newEmptyMVar, putMVar, takeMVar )
import UnliftIO.STM
    ( newTVarIO, readTVarIO, writeTVar )
import UnliftIO.Temporary
    ( withSystemTempDirectory, withSystemTempFile )

import qualified Cardano.Wallet.Address.Derivation.Shelley as Seq
import qualified Cardano.Wallet.DB.Sqlite.Schema as DB
import qualified Cardano.Wallet.DB.Sqlite.Types as DB
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.Persist.Sqlite as Sqlite
import qualified UnliftIO.STM as STM

spec :: Spec
spec =
    describe "LayerSpec"
        $ do
            stateMachineSpecSeq
            stateMachineSpecRnd
            stateMachineSpecShared
            propertiesSpecSeq
            loggingSpec
            fileModeSpec
            manualMigrationsSpec

stateMachineSpec
    :: forall k s .
        ( PersistAddressBook s
        , TestConstraints s k
        , Typeable s
        )
    => Spec
stateMachineSpec = describe ("State machine test (" ++ showState @s ++ ")") $ do
    validateGenerators @s
    let newDB = newDBLayerInMemory @s @k nullTracer dummyTimeInterpreter
    it "Sequential" $ prop_sequential newDB

stateMachineSpecSeq, stateMachineSpecRnd, stateMachineSpecShared :: Spec
stateMachineSpecSeq =
    stateMachineSpec @ShelleyKey @(SeqState 'Mainnet ShelleyKey)
stateMachineSpecRnd =
    stateMachineSpec @ByronKey @(RndState 'Mainnet)
stateMachineSpecShared =
    stateMachineSpec @SharedKey @(SharedState 'Mainnet SharedKey)

instance PaymentAddress SharedKey 'CredFromScriptK where
    paymentAddress _ = error
        "does not make sense for SharedKey but want to use stateMachineSpec"
    liftPaymentAddress _ = error
        "does not make sense for SharedKey but want to use stateMachineSpec"

showState :: forall s. Typeable s => String
showState = show (typeOf @s undefined)

withFreshDB
    :: (MonadIO m )
    => WalletId
    -> (DBLayer IO (SeqState 'Mainnet ShelleyKey) ShelleyKey -> m ())
    -> m ()
withFreshDB wid f = do
    (kill, db) <-
        liftIO $ newDBLayerInMemory nullTracer dummyTimeInterpreter wid
    f db
    liftIO kill

propertiesSpecSeq :: SpecWith ()
propertiesSpecSeq = describe "Properties" $ properties withFreshDB

{-------------------------------------------------------------------------------
                                Logging Spec
-------------------------------------------------------------------------------}

loggingSpec :: Spec
loggingSpec = withLoggingDB @(SeqState 'Mainnet ShelleyKey) $ do
    describe "Sqlite query logging" $ do
        it "should log queries at DEBUG level" $ \(getLogs, DBLayer{..}) -> do
            atomically $ unsafeRunExceptT $
                initializeWallet testCpSeq testMetadata mempty gp
            logs <- getLogs
            logs `shouldHaveMsgQuery` "INSERT"

        it "should not log query parameters" $ \(getLogs, DBLayer{..}) -> do
            atomically $ unsafeRunExceptT $
                initializeWallet testCpSeq testMetadata mempty gp
            let walletName = T.unpack $ coerce $ name testMetadata
            msgs <- T.unlines . mapMaybe getMsgQuery <$> getLogs
            T.unpack msgs `shouldNotContain` walletName

    describe "Sqlite observables" $ do
        it "should measure query timings" $ \(getLogs, DBLayer{..}) -> do
            let count = 5
            replicateM_ count (atomically readCheckpoint)
            msgs <- findObserveDiffs <$> getLogs
            length msgs `shouldBe` count * 2

withLoggingDB
    :: PersistAddressBook s
    => SpecWith (IO [DBLog], DBLayer IO s ShelleyKey)
    -> Spec
withLoggingDB = around f . beforeWith clean
  where
    f act = do
        logVar <- newTVarIO []
        withDBLayerInMemory
            (traceInTVarIO logVar)
            dummyTimeInterpreter
            testWid
            (\db -> act (logVar, db))
    clean (logs, db) = do
        STM.atomically $ writeTVar logs []
        pure (mapMaybe getMsgDB <$> readTVarIO logs, db)

getMsgDB :: WalletDBLog -> Maybe DBLog
getMsgDB (MsgDB msg) = Just msg

shouldHaveMsgQuery :: [DBLog] -> Text -> Expectation
shouldHaveMsgQuery msgs str = unless (any match msgs) $
    fail $ "Did not find DB query " ++
        T.unpack str ++ " within " ++ show msgs
  where
    match = maybe False (str `T.isInfixOf`) . getMsgQuery

getMsgQuery :: DBLog -> Maybe Text
getMsgQuery (MsgQuery msg _) = Just msg
getMsgQuery _ = Nothing

findObserveDiffs :: [DBLog] -> [DBLog]
findObserveDiffs = filter isObserveDiff
  where
    isObserveDiff (MsgRun _) = True
    isObserveDiff _ = False

{-------------------------------------------------------------------------------
                                File Mode Spec
-------------------------------------------------------------------------------}

type TestDBSeq = DBLayer IO (SeqState 'Mainnet ShelleyKey) ShelleyKey

fileModeSpec :: Spec
fileModeSpec =  do
    describe "Check db opening/closing" $ do
        it "Opening and closing of db works" $ do
            replicateM_ 25 $ do
                db <- temporaryDBFile
                withShelleyFileDBLayer @(SeqState 'Mainnet ShelleyKey) db
                    (\_ -> pure ())

    describe "DBFactory" $ do
        let ti = dummyTimeInterpreter
        let withDBFactory action = withSystemTempDirectory "DBFactory" $ \dir -> do
                dbf <- newDBFactory nullTracer defaultFieldValues ti (Just dir)
                action dir dbf

        let whileFileOpened delay f action = do
                opened <- newEmptyMVar
                concurrently_
                    (withFile f ReadMode (\_ -> putMVar opened () >> threadDelay delay))
                    (takeMVar opened >> action)

        it "withDatabase *> removeDatabase works and remove files" $ do
            withDBFactory $ \dir DBFactory{..} -> do
                -- NOTE
                -- Start a concurrent worker which makes action on the DB in
                -- parallel to simulate activity.
                pid <- forkIO $ withDatabase testWid $ \(DBLayer{..} :: TestDBSeq) -> do
                    handle @IO @SomeException (const (pure ())) $ forever $ do
                        atomically $ do
                            liftIO $ threadDelay 10_000
                            void readCheckpoint

                killThread pid *> removeDatabase testWid
                listDirectory dir `shouldReturn` mempty

        it "removeDatabase still works if file is opened" $ do
            withDBFactory $ \dir DBFactory{..} -> do
                -- set up a database file
                withDatabase testWid $ \(_ :: TestDBSeq) -> pure ()
                files <- listDirectory dir
                files `shouldNotBe` mempty

                -- Try removing the database when it's already opened for
                -- reading for 100ms.
                -- This simulates an antivirus program on windows which may
                -- interfere with file deletion.
                whileFileOpened 100_000 (dir </> head files) (removeDatabase testWid)
                listDirectory dir `shouldReturn` mempty

        it "removeDatabase waits for connections to close" $ do
            withDBFactory $ \_ DBFactory{..} -> do
                closed <- newEmptyMVar

                let conn =
                        withDatabase testWid $ \(_ :: TestDBSeq) -> do
                            threadDelay 500_000
                            putMVar closed ()
                let rm = do
                        removeDatabase testWid
                        isEmptyMVar closed

                concurrently conn (threadDelay 50_000 >> rm)
                    `shouldReturn` ((), False)

    describe "Sqlite database file" $ do
        let writeSomething DBLayer{..} = do
                atomically $ unsafeRunExceptT $
                    initializeWallet testCpSeq testMetadata mempty gp
            tempFilesAbsent fp = do
                doesFileExist fp `shouldReturn` True
                doesFileExist (fp <> "-wal") `shouldReturn` False
                doesFileExist (fp <> "-shm") `shouldReturn` False
            bomb = throwIO (userError "bomb")
        it "is properly closed after withDBLayer" $
            withTestDBFile writeSomething tempFilesAbsent
        it "is properly closed after an exception in withDBLayer" $
            withTestDBFile (\db -> writeSomething db >> bomb) tempFilesAbsent
                `shouldThrow` isUserError

    before temporaryDBFile $
        describe "Check db reading/writing from/to file and cleaning" $ do

        it "create and list wallet works" $ \f -> do
            withShelleyFileDBLayer f $ \DBLayer{..} -> do
                atomically $ unsafeRunExceptT $
                    initializeWallet testCp testMetadata mempty gp
            testReopening f getWalletId' testWid

        it "create and get meta works" $ \f -> do
            meta <- withShelleyFileDBLayer f $ \DBLayer{..} -> do
                now <- getCurrentTime
                let meta = testMetadata
                       { passphraseInfo = Just $ WalletPassphraseInfo now EncryptWithPBKDF2 }
                atomically $ unsafeRunExceptT $
                    initializeWallet testCp meta mempty gp
                return (meta, WalletDelegation NotDelegating [])
            testReopening f readWalletMeta' (Just meta)

        it "create and get private key" $ \f -> do
            (k, h) <- withShelleyFileDBLayer f $ \db@DBLayer{..} -> do
                atomically $ unsafeRunExceptT $
                    initializeWallet testCp testMetadata mempty gp
                unsafeRunExceptT $ attachPrivateKey db testWid
            testReopening f readPrivateKey' (Just (k, h))

        it "put and read tx history (Ascending)" $ \f -> do
            withShelleyFileDBLayer f $ \DBLayer{..} -> do
                atomically $ do
                    unsafeRunExceptT $
                        initializeWallet testCp testMetadata mempty gp
                    unsafeRunExceptT $ putTxHistory testTxs
            testReopening
                f
                (\db' ->
                    readTransactions' db' Ascending wholeRange Nothing
                    )
                testTxs -- expected after opening db

        it "put and read tx history (Descending)" $ \f -> do
            withShelleyFileDBLayer f $ \DBLayer{..} -> do
                atomically $ do
                    unsafeRunExceptT $
                        initializeWallet testCp testMetadata mempty gp
                    unsafeRunExceptT $ putTxHistory testTxs
            testReopening
                f
                (\db' ->
                    readTransactions' db' Descending wholeRange Nothing
                    )
                (reverse testTxs) -- expected after opening db

        it "put and read checkpoint" $ \f -> do
            withShelleyFileDBLayer f $ \DBLayer{..} -> do
                atomically $ do
                    unsafeRunExceptT $
                        initializeWallet testCp testMetadata mempty gp
                    unsafeRunExceptT $ putCheckpoint testCp
            testReopening f readCheckpoint' (Just testCp)

        describe "Golden rollback scenarios" $ do
            let dummyHash x = Hash $
                    x <> BS.pack (replicate (32 - (BS.length x)) 0)
            let dummyAddr x = Address $
                    x <> BS.pack (replicate (32 - (BS.length x)) 0)

            let mockApply DBLayer{..} h mockTxs = do
                    Just cpA <- atomically readCheckpoint
                    let slotA = view #slotNo $ currentTip cpA
                    let Quantity bhA = view #blockHeight $ currentTip cpA
                    let hashA = headerHash $ currentTip cpA
                    let fakeBlock = Block
                            (BlockHeader
                            { slotNo = slotA + 100
                            -- Increment blockHeight by steps greater than k
                            -- Such that old checkpoints are always pruned.
                            , blockHeight = Quantity $ bhA + 5_000
                            , headerHash = h
                            , parentHeaderHash = Just hashA
                            })
                            mockTxs
                            mempty
                    let (FilteredBlock{transactions=txs}, (_,cpB)) =
                            applyBlock fakeBlock cpA
                    atomically $ do
                        unsafeRunExceptT $ putCheckpoint cpB
                        unsafeRunExceptT $ putTxHistory txs
                        unsafeRunExceptT
                            $ prune (Quantity 2_160) $ 2_160 * 3 * 20

            it "Should spend collateral inputs and create spendable collateral \
                \outputs if validation fails" $
                \f -> withShelleyFileDBLayer f $ \db@DBLayer{..} -> do

                    let ourAddrs =
                            map (\(a,s,_) -> (a,s)) $
                            knownAddresses (getState testCp)

                    atomically $ unsafeRunExceptT $ initializeWallet
                        testCp testMetadata mempty gp

                    ------------------------------------------------------------
                    -- Transaction 1
                    --
                    -- This transaction provides initial funding for the wallet.
                    ------------------------------------------------------------

                    mockApply db (dummyHash "block1")
                        [ Tx
                            { txId = dummyHash "tx1"
                            , txCBOR = Nothing
                            , fee = Nothing
                            , resolvedInputs =
                                [ ( TxIn (dummyHash "faucet") 0
                                  , Just $ TxOut
                                        (dummyAddr "faucetOut1")
                                        (coinToBundle 4)
                                  )
                                , ( TxIn (dummyHash "faucet") 1
                                  , Just $ TxOut
                                        (dummyAddr "faucetOut2")
                                        (coinToBundle 8)
                                  )
                                ]
                            , resolvedCollateralInputs = []
                            , outputs =
                                [ TxOut
                                    (fst $ head ourAddrs)
                                    (coinToBundle 4)
                                , TxOut
                                    (fst $ head $ tail ourAddrs)
                                    (coinToBundle 8)
                                ]
                            , collateralOutput = Nothing
                            , withdrawals = mempty
                            , metadata = Nothing
                            , scriptValidity = Just TxScriptValid
                            }
                        ]
                    getAvailableBalance db `shouldReturn` 12 -- == (4 + 8)

                    ------------------------------------------------------------
                    -- Transaction 2
                    --
                    -- This transaction has a script that fails validation.
                    -- Therefore, we should forfeit value from the collateral
                    -- inputs, but recover value from the collateral outputs.
                    ------------------------------------------------------------

                    mockApply db (dummyHash "block2")
                        [ Tx
                            { txId = dummyHash "tx2"
                            , txCBOR = Nothing
                            , fee = Nothing
                            , resolvedInputs =
                                [ ( TxIn (dummyHash "tx1") 0
                                  , Just $ TxOut
                                        (dummyAddr "faucetOut1")
                                        (coinToBundle 4)
                                  )
                                ]
                            , resolvedCollateralInputs =
                                [(TxIn (dummyHash "tx1") 1, Nothing)]
                            , outputs =
                                [ TxOut
                                    (dummyAddr "faucetAddr2") (coinToBundle 2)
                                , TxOut
                                    (fst $ ourAddrs !! 1) (coinToBundle 2)
                                ]
                            , collateralOutput =
                                Just $ TxOut
                                    (fst $ ourAddrs !! 1) (coinToBundle 7)
                            , withdrawals = mempty
                            , metadata = Nothing
                            , scriptValidity = Just TxScriptInvalid
                            }
                        ]
                    mockApply db (dummyHash "block2a") []
                    getTxsInLedger db `shouldReturn`
                        -- We:
                        -- - forfeited 8 from collateral inputs;
                        -- - recovered 7 from collateral ouputs.
                        -- Therefore we lost a net collateral value of (8 - 7):
                        [ (Outgoing, 1)
                        -- We got 12 from the faucet
                        , (Incoming, 12)
                        ]
                    getAvailableBalance db `shouldReturn` 11 -- = 12 - 1

                    ------------------------------------------------------------
                    -- Transaction 3
                    --
                    -- This transaction uses a collateral output created in the
                    -- previous transaction to make a payment.
                    ------------------------------------------------------------

                    mockApply db (dummyHash "block3")
                        [ Tx
                            { txId = dummyHash "tx3"
                            , txCBOR = Nothing
                            , fee = Nothing
                            , resolvedInputs =
                                -- Here we refer to a collateral output from
                                -- the previous transaction.
                                --
                                -- Note that we refer to the sole collateral
                                -- output by using a special index value that
                                -- is equal to the number of ordinary outputs
                                -- in that transaction:
                                --
                                [ ( TxIn (dummyHash "tx2") 2
                                  , Just $ TxOut
                                        (fst $ ourAddrs !! 1)
                                        (coinToBundle 7)
                                  )
                                ]
                            , resolvedCollateralInputs =
                                []
                            , outputs =
                                [ TxOut
                                    (dummyAddr "faucetAddr2") (coinToBundle 8)
                                ]
                            , collateralOutput = Nothing
                            , withdrawals = mempty
                            , metadata = Nothing
                            , scriptValidity = Just TxScriptValid
                            }
                        ]
                    mockApply db (dummyHash "block3a") []
                    getAvailableBalance db `shouldReturn` 4 -- = 11 - 7

            it "(Regression test #1575) - TxMetas and checkpoints should \
               \rollback to the same place" $ \f -> do
              withShelleyFileDBLayer f $ \db@DBLayer{..} -> do

                let ourAddrs =
                        map (\(a,s,_) -> (a,s)) $
                        knownAddresses (getState testCp)

                atomically $ unsafeRunExceptT $ initializeWallet
                    testCp testMetadata mempty gp

                let mockApplyBlock1 = mockApply db (dummyHash "block1")
                        [ Tx
                            { txId = dummyHash "tx1"
                            , txCBOR = Nothing
                            , fee = Nothing
                            , resolvedInputs =
                                [ ( TxIn (dummyHash "faucet") 0
                                  , Just $ TxOut
                                        (dummyAddr "out_for_in")
                                        (coinToBundle 4)
                                  )
                                ]
                            -- TODO: (ADP-957)
                            , resolvedCollateralInputs = []
                            , outputs =
                                [TxOut (fst $ head ourAddrs) (coinToBundle 4)]
                            , collateralOutput = Nothing
                            , withdrawals = mempty
                            , metadata = Nothing
                            , scriptValidity = Nothing
                            }
                        ]

                -- Slot 1 0
                mockApplyBlock1
                getAvailableBalance db `shouldReturn` 4

                -- Slot 200
                mockApply db (dummyHash "block2a")
                    [ Tx
                        { txId = dummyHash "tx2a"
                        , txCBOR = Nothing
                        , fee = Nothing
                        , resolvedInputs =
                            [ ( TxIn (dummyHash "tx1") 0
                                , Just $ TxOut
                                    (dummyAddr "out_for_in")
                                    (coinToBundle 4)
                                )
                            ]
                        -- TODO: (ADP-957)
                        , resolvedCollateralInputs = []
                        , outputs =
                            [ TxOut (dummyAddr "faucetAddr2") (coinToBundle 2)
                            , TxOut (fst $ ourAddrs !! 1) (coinToBundle 2)
                            ]
                        , collateralOutput = Nothing
                        , withdrawals = mempty
                        , metadata = Nothing
                        , scriptValidity = Nothing
                        }
                    ]

                -- Slot 300
                mockApply db (dummyHash "block3a") []
                getAvailableBalance db `shouldReturn` 2
                getTxsInLedger db `shouldReturn` [(Outgoing, 2), (Incoming, 4)]

                atomically . void . unsafeRunExceptT $
                    rollbackTo (At $ SlotNo 200)
                Just cp <- atomically readCheckpoint
                view #slotNo (currentTip cp) `shouldBe` (SlotNo 0)

                getTxsInLedger db `shouldReturn` []

    describe "random operation chunks property" $ do
        it "realize a random batch of operations upon one db open"
            (property $ prop_randomOpChunks @(SeqState 'Mainnet ShelleyKey))

-- This property checks that executing series of wallet operations in a single
-- SQLite session has the same effect as executing the same operations over
-- multiple sessions.
prop_randomOpChunks
    :: (Eq s, PersistAddressBook s, Show s)
    =>  NonEmptyList (Wallet s, WalletMetadata)
    -> Property
prop_randomOpChunks (NonEmpty []) = error "arbitrary generated an empty list"
prop_randomOpChunks (NonEmpty (p : pairs)) =
    not (null pairs) ==> monadicIO (liftIO prop)
  where
    prop = do
        filepath <- temporaryDBFile
        withShelleyFileDBLayer filepath $ \dbF -> do
            withShelleyDBLayer $ \dbM -> do
                boot dbM p
                boot dbF p
                forM_ pairs (insertPair dbM)
                cutRandomly pairs >>= mapM_ (mapM (insertPair dbF))
                dbF `shouldBeConsistentWith` dbM
    boot DBLayer{..} (cp, meta) = do
        let cp0 = imposeGenesisState cp
        atomically $ unsafeRunExceptT $ initializeWallet cp0 meta mempty gp

    insertPair
        :: DBLayer IO s k
        -> (Wallet s, WalletMetadata)
        -> IO ()
    insertPair DBLayer{..} (cp, meta) = atomically $ do
            unsafeRunExceptT $ putCheckpoint cp
            unsafeRunExceptT $ putWalletMeta meta

    imposeGenesisState :: Wallet s -> Wallet s
    imposeGenesisState = over #currentTip $ \(BlockHeader _ _ h _) ->
        BlockHeader (SlotNo 0) (Quantity 0) h Nothing

    shouldBeConsistentWith :: (Eq s, Show s) => DBLayer IO s k -> DBLayer IO s k -> IO ()
    shouldBeConsistentWith db1 db2 = do
        walId1 <-  getWalletId' db1
        walId2 <-  getWalletId' db2
        walId1 `shouldBe` walId2

        cps1 <- readCheckpoint' db1
        cps2 <- readCheckpoint' db2
        cps1 `shouldBe` cps2

        meta1 <- readWalletMeta' db1
        meta2 <- readWalletMeta' db2
        meta1 `shouldBe` meta2

-- | Test that data is preserved when closing the database and opening
-- it again.
testReopening
    :: (Show s, Eq s)
    => FilePath
    -> (DBLayer IO (SeqState 'Mainnet ShelleyKey) ShelleyKey -> IO s)
    -> s
    -> Expectation
testReopening filepath call expectedAfterOpen = do
    withShelleyFileDBLayer filepath $ \db -> do
        call db `shouldReturn` expectedAfterOpen

-- | Run a test action inside withDBLayer, then check assertions.
withTestDBFile
    :: (DBLayer IO (SeqState 'Mainnet ShelleyKey) ShelleyKey -> IO ())
    -> (FilePath -> IO a)
    -> IO a
withTestDBFile action expectations = do
    logConfig <- defaultConfigTesting
    trace <- setupTrace (Right logConfig) "connectionSpec"
    withSystemTempFile "spec.db" $ \fp h -> do
        hClose h
        removeFile fp
        withDBLayer
            (trMessageText trace)
            (Just defaultFieldValues)
            fp
            ti
            testWid
            action
        expectations fp
  where
    ti = dummyTimeInterpreter

temporaryDBFile :: IO FilePath
temporaryDBFile = emptySystemTempFile "cardano-wallet-SqliteFileMode"

defaultFieldValues :: DefaultFieldValues
defaultFieldValues = DefaultFieldValues
    { defaultActiveSlotCoefficient = ActiveSlotCoefficient 1.0
    , defaultDesiredNumberOfPool = 0
    , defaultMinimumUTxOValue = Coin 1_000_000
    , defaultHardforkEpoch = Nothing
    , defaultKeyDeposit = Coin 2_000_000
    }

-- Note: Having helper with concrete key types reduces the need
-- for type-application everywhere.
withShelleyDBLayer
    :: PersistAddressBook s
    => (DBLayer IO s ShelleyKey -> IO a)
    -> IO a
withShelleyDBLayer = withDBLayerInMemory nullTracer dummyTimeInterpreter testWid

withShelleyFileDBLayer
    :: PersistAddressBook s
    => FilePath
    -> (DBLayer IO s ShelleyKey -> IO a)
    -> IO a
withShelleyFileDBLayer fp = withDBLayer
    nullTracer  -- fixme: capture logging
    (Just defaultFieldValues)
    fp
    dummyTimeInterpreter
    testWid

getWalletId'
    :: Applicative m
    => DBLayer m s k
    -> m WalletId
getWalletId' DBLayer{..} = pure walletId_

readCheckpoint'
    :: DBLayer m s k
    -> m (Maybe (Wallet s))
readCheckpoint' DBLayer{..} = atomically readCheckpoint

readWalletMeta'
    :: DBLayer m s k
    -> m (Maybe (WalletMetadata, WalletDelegation))
readWalletMeta' DBLayer{..} = atomically readWalletMeta

readTransactions'
    :: DBLayer m s k
    -> SortOrder
    -> Range SlotNo
    -> Maybe TxStatus
    -> m [(Tx, TxMeta)]
readTransactions' DBLayer{..} a1 a2 mstatus =
    atomically . fmap (fmap toTxHistory)
        $ readTransactions Nothing a1 a2 mstatus Nothing

readPrivateKey'
    :: DBLayer m s k
    -> m (Maybe (k 'RootK XPrv, PassphraseHash))
readPrivateKey' DBLayer{..} = atomically readPrivateKey

-- | Attach an arbitrary private key to a wallet
attachPrivateKey
    :: DBLayer IO s ShelleyKey
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO (ShelleyKey 'RootK XPrv, PassphraseHash)
attachPrivateKey DBLayer{..} wid = do
    let pwd = Passphrase $ BA.convert $ T.encodeUtf8 "simplevalidphrase"
    seed <- liftIO $ generate $ SomeMnemonic <$> genMnemonic @15
    (scheme, h) <- liftIO $ encryptPassphrase pwd
    let k = generateKeyFromSeed (seed, Nothing) (preparePassphrase scheme pwd)
    mkNoSuchWalletError wid $ mapExceptT atomically $ putPrivateKey (k, h)
    return (k, h)

cutRandomly :: [a] -> IO [[a]]
cutRandomly = iter []
  where
    iter acc rest
        | L.length rest <= 1 =
            pure $ L.reverse (rest:acc)
        | otherwise = do
            chunksNum <- randomRIO (1, L.length rest)
            let chunk = L.take chunksNum rest
            iter (chunk:acc) (L.drop chunksNum rest)


{-------------------------------------------------------------------------------
                            Manual migrations tests
-------------------------------------------------------------------------------}

manualMigrationsSpec :: Spec
manualMigrationsSpec = describe "Manual migrations" $ do
    it "'migrate' db with no passphrase scheme set."
        testMigrationPassphraseScheme1
    it "not 'migrate' db with no passphrase scheme set."
        testMigrationPassphraseScheme2
    it "not 'migrate' db with no date."
        testMigrationPassphraseScheme3
    it "not 'migrate' db with no passphrase scheme set and no date."
        testMigrationPassphraseScheme4

    it "'migrate' db with no 'derivation_prefix' for seq state (Icarus)" $
        testMigrationSeqStateDerivationPrefix @IcarusKey
            "icarusDerivationPrefix-v2020-10-07.sqlite"
            ( purposeBIP44
            , coinTypeAda
            , minBound
            )

    it "'migrate' db with no 'derivation_prefix' for seq state (Shelley)" $
        testMigrationSeqStateDerivationPrefix @ShelleyKey
            "shelleyDerivationPrefix-v2020-10-07.sqlite"
            ( purposeCIP1852
            , coinTypeAda
            , minBound
            )

    it "'migrate' db with old text serialization for 'Role'" $
        testMigrationRole
            "shelleyRole-v2020-10-13.sqlite"

    it "'migrate' db with partially applied checkpoint migration" $
        testMigrationRole
            "shelleyRole-corrupted-v2020-10-13.sqlite"

    it "'migrate' db with unused protocol parameters in checkpoints" $
        testMigrationCleanupCheckpoints
            "shelleyDerivationPrefix-v2020-10-07.sqlite"
            (GenesisParameters
                { getGenesisBlockHash = Hash $ unsafeFromHex
                    "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"
                , getGenesisBlockDate =
                    StartTime $ posixSecondsToUTCTime 1_506_203_091
                }
            )
            (BlockHeader
                { slotNo = SlotNo 1_125_119
                , blockHeight = Quantity 1_124_949
                , headerHash = Hash $ unsafeFromHex
                    "3b309f1ca388459f0ce2c4ccca20ea646b75e6fc1447be032a41d43f209ecb50"
                , parentHeaderHash = Just $ Hash $ unsafeFromHex
                    "e9414e08d8c5ca177dd0cb6a9e4bf868e1ea03389c31f5f7a6b099a3bcdfdedf"
                }
            )

    it "'migrate' db to add fees to transactions" $
        testMigrationTxMetaFee
            "metaFee-v2020-11-26.sqlite"
            129 -- number of transactions

            -- This one (chosen for its stake key registration) has:
            --
            -- - one input of 1000000000
            -- - one output of 997825743.
            --
            -- which gives a delta of 2174257, which means
            --
            -- - 2000000 of key deposit
            -- -  174257 of fee
            --
            [ ( Hash $ unsafeFromHex "00058d433a73574a64d0b4a3c37f1e460697fa8f1e3265a51e95eb9e9573b5ab"
              , Coin 174_257
              )

            -- This one (chosen because of its very round fee) has:
            --
            -- - two inputs of 1000000 each
            -- - one output of 1000000
            --
            -- which gives a delta (and fee) of 1000000

            , ( Hash $ unsafeFromHex "8f79e7f79ddeb7a7494121259832c0180c1b6bb746d8b2337cd1f4fb5b0d8216"
              , Coin 1_000_000
              )

            -- This one (chosen for its withdrawal) has:
            --
            -- - one input of 909199523
            -- - one withdrawal of 863644
            -- - two outputs of 1000000 and 908888778
            --
            -- which gives a delta (and fee) of 174389

            , ( Hash $ unsafeFromHex "eefa06dfa8ce91237117f9b4bdc4f6970c31de54906313b545dafb7ca6235171"
              , Coin 174_389
              )

            -- This one (chosen for its high fee) has:
            --
            -- - one input of 997825743
            -- - two outputs of 1000000 and 995950742
            --
            -- which gives a delta (and fee) of 875001
            , ( Hash $ unsafeFromHex "8943f9fa4b56b32cd44ab9c22d46693882f0bbca1bc3f0705124e75c2e40b9c2"
              , Coin 875_001
              )

            -- This one (chosen for having many inputs and many outputs) has:
            --
            -- - 10 inputs:
            --     - 1654330
            --     - 2111100
            --     - 2234456
            --     - 9543345
            --     - 1826766
            --     - 8871831
            --     - 3823766
            --     - 6887025
            --     - 1958037
            --     - 3575522
            --
            -- - 10 outputs:
            --      - 4000000
            --      - 7574304
            --      - 9000000
            --      - 1000000
            --      - 1164635
            --      - 6752132
            --      - 1000000
            --      - 8596880
            --      - 2000000
            --      - 1707865
            --
            -- - 1 withdrawal:
            --      - 565251
            --
            -- which gives a delta (and fee) of 255613
            , ( Hash $ unsafeFromHex "99907bf6ac73f6fe6fe25bd6b68bae6776425b9d15a7c46c7a49b85b8b03f291"
              , Coin 255_613
              )

            -- This one (chosen for its high ratio input:output) has:
            --
            -- - 1 input of 1000000000
            -- - 33 relatively small outputs
            -- - 1 withdrawal of 561120
            --
            -- which gives a delta (and fee) of 267537
            , ( Hash $ unsafeFromHex "15940a7c1df8696279282046ebdb1ee890d4e9ac3c5d7213f360921648b36666"
              , Coin 267_537
              )
            ]

    it "'migrate' db to create metadata table when it doesn't exist"
        testCreateMetadataTable

    it "'migrate' db never modifies database with newer version"
        testNewerDatabaseIsNeverModified

    it "'migrate' db submissions encoding" $
        testMigrationSubmissionsEncoding
            "before_submission-v2022-12-14.sqlite"

-- | Copy a given @.sqlite@ file, load it into a `DBLayer`
-- (possibly triggering migrations), and run an action on it.
--
-- Useful for testing the logs and results of migrations.
withDBLayerFromCopiedFile
    :: forall k s a.
        ( PersistAddressBook s
        , PersistPrivateKey (k 'RootK)
        , WalletKey k
        , s ~ SeqState 'Mainnet k
        )
    => FilePath
        -- ^ Filename of the @.sqlite@ file to load.
    -> (DBLayer IO s k -> IO a)
        -- ^ Action to run.
    -> IO ([WalletDBLog], a)
        -- ^ (logs, result of the action)
withDBLayerFromCopiedFile dbName action = withinCopiedFile dbName
    $ \path tr -> withDBOpenFromFile tr (Just defaultFieldValues) path
    $ \db -> do
        mwid <- retrieveWalletId db
        case mwid of
            Nothing -> fail "No wallet id found in database"
            Just wid ->
                withDBLayerFromDBOpen @k @s dummyTimeInterpreter wid action db

withinCopiedFile
    :: FilePath
    -> (FilePath -> Tracer IO msg -> IO a) -> IO ([msg], a)
withinCopiedFile dbName action = do
    let orig = $(getTestData) </> dbName
    withSystemTempDirectory "migration-db" $ \dir -> do
        let path = dir </> "db.sqlite"
        copyFile orig path
        captureLogging $ action path

testMigrationTxMetaFee
    :: String
    -> Int
    -> [(Hash "Tx", Coin)]
    -> IO ()
testMigrationTxMetaFee dbName expectedLength caseByCase = do
    (logs, result) <- withDBLayerFromCopiedFile @ShelleyKey dbName
        $ \DBLayer{..} -> atomically $ do
            readTransactions
                Nothing Descending wholeRange Nothing Nothing

    -- Check that we've indeed logged a needed migration for 'fee'
    length (filter isMsgManualMigration logs) `shouldBe` 1

    -- Check that the migrated history has the correct length.
    length result `shouldBe` expectedLength

    -- Verify that all incoming transactions have no fees set, and that all
    -- outgoing ones do.
    forM_ result $ \TransactionInfo{txInfoFee,txInfoMeta} -> do
        case txInfoMeta ^. #direction of
            Incoming -> txInfoFee `shouldSatisfy` isNothing
            Outgoing -> txInfoFee `shouldSatisfy` isJust

    -- Also verify a few hand-picked transactions
    forM_ caseByCase $ \(txid, expectedFee) -> do
        case L.find ((== txid) . txInfoId) result of
            Nothing ->
                fail $ "tx not found: " <> T.unpack (toText txid)
            Just TransactionInfo{txInfoFee} ->
                txInfoFee `shouldBe` Just expectedFee
  where
    isMsgManualMigration = matchMsgManualMigration $ \field ->
        let fieldInDB = fieldDB $ persistFieldDef DB.TxMetaFee
        in fieldName field == unFieldNameDB fieldInDB

matchMsgManualMigration :: (DBField -> Bool) -> WalletDBLog -> Bool
matchMsgManualMigration p = \case
    MsgDB (MsgManualMigrationNeeded field _) -> p field
    MsgDB (MsgExpectedMigration (MsgManualMigrationNeeded field _)) -> p field
    _ -> False

testMigrationCleanupCheckpoints
    :: FilePath
    -> GenesisParameters
    -> BlockHeader
    -> IO ()
testMigrationCleanupCheckpoints dbName genesisParameters tip = do
    (logs, result) <- withDBLayerFromCopiedFile @ShelleyKey dbName
        $ \DBLayer{..} -> atomically $ do
            (,) <$> readGenesisParameters
                <*> readCheckpoint

    length (filter (isMsgManualMigration fieldGenesisHash) logs) `shouldBe` 1
    length (filter (isMsgManualMigration fieldGenesisStart) logs) `shouldBe` 1

    (fst result) `shouldBe` Just genesisParameters
    (currentTip <$> snd result) `shouldBe` Just tip
  where
    fieldGenesisHash = fieldDB $ persistFieldDef DB.WalGenesisHash
    fieldGenesisStart = fieldDB $ persistFieldDef DB.WalGenesisStart

    isMsgManualMigration :: FieldNameDB -> WalletDBLog -> Bool
    isMsgManualMigration fieldInDB = matchMsgManualMigration $ \field ->
        fieldName field == unFieldNameDB fieldInDB

testMigrationRole
    :: String
    -> IO ()
testMigrationRole dbName = do
    (logs, Just cp) <- withDBLayerFromCopiedFile @ShelleyKey dbName
        $ \DBLayer{..} -> atomically $ do
            readCheckpoint

    let migrationMsg = filter isMsgManualMigration logs
    length migrationMsg `shouldBe` 3
    length (knownAddresses $ getState cp) `shouldBe` 71
  where
    isMsgManualMigration :: WalletDBLog -> Bool
    isMsgManualMigration = matchMsgManualMigration $ \field ->
        let fieldInDB = fieldDB $ persistFieldDef DB.SeqStateAddressRole
        in fieldName field == unFieldNameDB fieldInDB

testMigrationSeqStateDerivationPrefix
    :: forall k s.
        ( s ~ SeqState 'Mainnet k
        , WalletKey k
        , PersistAddressBook s
        , PersistPrivateKey (k 'RootK)
        )
    => String
    -> ( Index 'Hardened 'PurposeK
       , Index 'Hardened 'CoinTypeK
       , Index 'Hardened 'AccountK
       )
    -> IO ()
testMigrationSeqStateDerivationPrefix dbName prefix = do
    (logs, Just cp) <- withDBLayerFromCopiedFile @k @s dbName
        $ \DBLayer{..} -> atomically $ do
            readCheckpoint

    let migrationMsg = filter isMsgManualMigration logs
    length migrationMsg `shouldBe` 1
    derivationPrefix (getState cp) `shouldBe` DerivationPrefix prefix
  where
    isMsgManualMigration = matchMsgManualMigration $ \field ->
        let fieldInDB = fieldDB $ persistFieldDef DB.SeqStateDerivationPrefix
        in fieldName field == unFieldNameDB fieldInDB

testMigrationPassphraseScheme1 :: IO ()
testMigrationPassphraseScheme1 = do
    -- The first wallet is stored in the database with only a
    -- 'passphraseLastUpdatedAt' field, but no 'passphraseScheme'. So,
    -- after the migration, both should now be `Just`.
    (logs, a) <- withDBLayerFromCopiedFile @ShelleyKey
        "passphraseScheme/she.17ca0ed41a372e483f2968aa386a4b6b0ca6b5ee.sqlite"
        $ \DBLayer{..} -> atomically $ do
            Just a <- readWalletMeta
            pure (fst a)

    -- Migration is visible from the logs
    let migrationMsg = filter isMsgManualMigrationPw logs
    length migrationMsg `shouldBe` 1
    (passphraseScheme <$> passphraseInfo a) `shouldBe` Just EncryptWithPBKDF2

testMigrationPassphraseScheme2 :: IO ()
testMigrationPassphraseScheme2 = do
    -- The second wallet was just fine and already has a passphrase
    -- scheme set to use PBKDF2. Nothing should have changed.
    (logs, a) <- withDBLayerFromCopiedFile @ShelleyKey
        "passphraseScheme/she.2e8353d2bb937445948669a1dcc69ec9628a558c.sqlite"
        $ \DBLayer{..} -> atomically $ do
            Just a <- readWalletMeta
            pure (fst a)

    let migrationMsg = filter isMsgManualMigrationPw logs
    length migrationMsg `shouldBe` 1
    (passphraseScheme <$> passphraseInfo a) `shouldBe` Just EncryptWithPBKDF2

testMigrationPassphraseScheme3 :: IO ()
testMigrationPassphraseScheme3 = do
    -- The third wallet had a scheme too, but was using the legacy
    -- scheme. Nothing should have changed.
    (logs, a) <- withDBLayerFromCopiedFile @ShelleyKey
        "passphraseScheme/she.899abf7137aa8b3200d55d70474f6fdd2649fa2f.sqlite"
        $ \DBLayer{..} -> atomically $ do
            Just a <- readWalletMeta
            pure (fst a)

    let migrationMsg = filter isMsgManualMigrationPw logs
    length migrationMsg `shouldBe` 1
    (passphraseScheme <$> passphraseInfo a) `shouldBe` Just EncryptWithScrypt

testMigrationPassphraseScheme4 :: IO ()
testMigrationPassphraseScheme4 = do
    -- The last wallet had no passphrase whatsoever (restored from
    -- account public key), so it should still have NO scheme.
    (logs, a ) <- withDBLayerFromCopiedFile @ShelleyKey
        "passphraseScheme/she.be92ab4ec9399449e53b94378e6cb6724691f8b3.sqlite"
        $ \DBLayer{..} -> atomically $ do
            Just a <- readWalletMeta
            pure (fst a)

    let migrationMsg = filter isMsgManualMigrationPw logs
    length migrationMsg `shouldBe` 1
    (passphraseScheme <$> passphraseInfo a) `shouldBe` Nothing

isMsgManualMigrationPw :: WalletDBLog -> Bool
isMsgManualMigrationPw = matchMsgManualMigration $ \field ->
    let fieldInDB = fieldDB $ persistFieldDef DB.WalPassphraseScheme
    in  fieldName field == unFieldNameDB fieldInDB


testCreateMetadataTable ::
    forall k. (k ~ ShelleyKey) => IO ()
testCreateMetadataTable = withSystemTempFile "db.sql" $ \path _ -> do
    let noop _ = pure ()
        tr = nullTracer
    withDBOpenFromFile @_ @k tr (Just defaultFieldValues) path noop
    actualVersion <- Sqlite.runSqlite (T.pack path) $ do
        [Sqlite.Single (version :: Int)] <- Sqlite.rawSql
            "SELECT version FROM database_schema_version \
            \WHERE name = 'schema'" []
        pure $ SchemaVersion $ fromIntegral version
    actualVersion `shouldBe` currentSchemaVersion

testNewerDatabaseIsNeverModified ::
    forall k. (k ~ ShelleyKey) => IO ()
testNewerDatabaseIsNeverModified = withSystemTempFile "db.sql" $ \path _ -> do
    let newerVersion = SchemaVersion 100
    _ <- Sqlite.runSqlite (T.pack path) $ do
        Sqlite.rawExecute
            "CREATE TABLE database_schema_version (name, version)" []
        Sqlite.rawExecute (
            "INSERT INTO database_schema_version \
            \VALUES ('schema', " <> T.pack (show newerVersion) <> ")"
            ) []
    let noop _ = pure ()
        tr = nullTracer
    withDBOpenFromFile @_ @k tr (Just defaultFieldValues) path noop
        `shouldThrow` \case
            InvalidDatabaseSchemaVersion {..}
                | expectedVersion == currentSchemaVersion
                && actualVersion == newerVersion -> True
            _ -> False

localTxSubmissionTableExists :: Text
localTxSubmissionTableExists = [i|
    SELECT EXISTS (
        SELECT
            name
        FROM
            sqlite_schema
        WHERE
            type='table' AND
            name='local_tx_submission'
    );
    |]

testMigrationSubmissionsEncoding
    :: FilePath -> IO ()
testMigrationSubmissionsEncoding dbName = do
    let performMigrations path =
          withDBLayer @(SeqState 'Mainnet ShelleyKey) @ShelleyKey
            nullTracer (Just defaultFieldValues) path dummyTimeInterpreter
                testWid $ \_ -> pure ()
        testOnCopiedAndMigrated test = fmap snd
            $ withinCopiedFile dbName $ \path _  -> do
                performMigrations path
                test path
    testOnCopiedAndMigrated testAllPresentTxIdsAreInLedger
    testOnCopiedAndMigrated allTransactionDataIsRepresentedInTxMeta
    testOnCopiedAndMigrated testLocalTxSubmissionIsMissing

    where

        testLocalTxSubmissionIsMissing path = do
            [Single (count :: Int)] <- Sqlite.runSqlite (T.pack path) $
                    Sqlite.rawSql localTxSubmissionTableExists []
            count `shouldBe` 0
            pure ()

        allTransactionDataIsRepresentedInTxMeta path = do
            let idOf = unSingle @DB.TxId
                selectIds t = fmap idOf
                    <$> Sqlite.rawSql ("SELECT tx_id FROM " <> t) []
                runQuery = Sqlite.runSqlite (T.pack path)
                shouldBe' mtxs t   = do
                    txs <- runQuery $ selectIds t
                    liftIO $
                        Set.fromList txs `shouldSatisfy`
                            \x -> x `Set.isSubsetOf` (Set.fromList mtxs)
            metaIds  <-  runQuery $ selectIds "tx_meta"
            mapM_ (shouldBe' metaIds)
                [ "tx_in"
                , "tx_collateral"
                , "tx_out"
                , "tx_out_token"
                , "tx_collateral_out"
                , "tx_collateral_out_token"
                , "tx_withdrawal"
                , "c_b_o_r"
                ]

        testAllPresentTxIdsAreInLedger path = do
            metas <- Sqlite.runSqlite (T.pack path)
                $ Sqlite.rawSql "SELECT status FROM tx_meta" []
            forM_ metas $ \(Single status) ->
                status `shouldBe` ("in_ledger" :: Text)
{-------------------------------------------------------------------------------
                                   Test data
-------------------------------------------------------------------------------}

coinToBundle :: Word64 -> TokenBundle
coinToBundle = TokenBundle.fromCoin . Coin.fromWord64

testCp :: Wallet (SeqState 'Mainnet ShelleyKey)
testCp = snd $ initWallet block0 initDummyState
  where
    initDummyState :: SeqState 'Mainnet ShelleyKey
    initDummyState = mkSeqStateFromRootXPrv (xprv, mempty) purposeCIP1852 defaultAddressPoolGap
      where
        mw = SomeMnemonic . unsafePerformIO . generate $ genMnemonic @15
        xprv = generateKeyFromSeed (mw, Nothing) mempty

testMetadata :: WalletMetadata
testMetadata = WalletMetadata
    { name = WalletName "test wallet"
    , creationTime = unsafePerformIO getCurrentTime
    , passphraseInfo = Nothing
    }

testWid :: WalletId
testWid = WalletId (hash ("test" :: ByteString))

testTxs :: [(Tx, TxMeta)]
testTxs = [ (tx1, meta1), (tx2, meta2) ]
  where
    tx1 = Tx
        { txId = mockHash @String "tx2"
        , txCBOR = Nothing
        , fee = Nothing
        , resolvedInputs =
            [ ( TxIn (mockHash @String "tx1") 0
              , Nothing
              )
            ]
        , resolvedCollateralInputs = []
        , outputs = [TxOut (Address "addr1") (coinToBundle 10)]
        , collateralOutput = Nothing
        , withdrawals = mempty
        , metadata = Nothing
        , scriptValidity = Nothing
        }
    meta1 = TxMeta
        { status = InLedger
        , direction = Incoming
        , slotNo = SlotNo 140
        , blockHeight = Quantity 0
        , amount = Coin 1_337_144
        , expiry = Nothing
        }
    tx2 = Tx
        { txId = mockHash @String "tx3"
        , txCBOR = Nothing
        , fee = Nothing
        , resolvedInputs =
            [ ( TxIn (mockHash @String "tx2") 0
              , headMay $ tx1 ^. #outputs
              )
            ]
        , resolvedCollateralInputs = []
        , outputs =
            [ TxOut (Address "addr2") (coinToBundle 5)
            , TxOut (Address "addr3") (coinToBundle 5)
            ]
        , collateralOutput = Nothing
        , withdrawals = mempty
        , metadata = Nothing
        , scriptValidity = Nothing
        }
    meta2 = TxMeta
        { status = InLedger
        , direction = Incoming
        , slotNo = SlotNo 150
        , blockHeight = Quantity 0
        , amount = Coin 10
        , expiry = Nothing
        }

gp :: GenesisParameters
gp = dummyGenesisParameters

{-------------------------------------------------------------------------------
                    Helpers for golden rollback tests
-------------------------------------------------------------------------------}

getAvailableBalance :: DBLayer IO s k -> IO Natural
getAvailableBalance DBLayer{..} = do
    cp <- fromMaybe (error "nothing") <$> atomically readCheckpoint
    pend <- atomically $ fmap toTxHistory
        <$> readTransactions Nothing Descending wholeRange
                (Just Pending) Nothing
    return $ fromIntegral $ unCoin $ TokenBundle.getCoin $
        availableBalance (Set.fromList $ map fst pend) cp

getTxsInLedger :: DBLayer IO s k -> IO ([(Direction, Natural)])
getTxsInLedger DBLayer {..} = do
    pend <- atomically $ fmap toTxHistory
        <$> readTransactions Nothing Descending wholeRange
                (Just InLedger) Nothing
    pure $ map (\(_, m) -> (direction m, fromIntegral $ unCoin $ amount m)) pend

{-------------------------------------------------------------------------------
                           Test data - Sequential AD
-------------------------------------------------------------------------------}

testCpSeq :: Wallet (SeqState 'Mainnet ShelleyKey)
testCpSeq = snd $ initWallet block0 initDummyStateSeq

initDummyStateSeq :: SeqState 'Mainnet ShelleyKey
initDummyStateSeq = mkSeqStateFromRootXPrv (xprv, mempty) purposeCIP1852 defaultAddressPoolGap
  where
      mw = SomeMnemonic $ unsafePerformIO (generate $ genMnemonic @15)
      xprv = Seq.generateKeyFromSeed (mw, Nothing) mempty
