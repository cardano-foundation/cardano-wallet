{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
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

{-# OPTIONS_GHC -Wno-unused-imports #-}
module Cardano.Wallet.DB.SqliteSpec
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
    ( DBField
    , DBLog (..)
    , DBLog (..)
    , SqliteContext
    , destroyDBLayer
    , fieldName
    , fieldName
    , newInMemorySqliteContext
    , newInMemorySqliteContext
    )
import Cardano.Mnemonic
    ( SomeMnemonic (..) )
import Cardano.Wallet.DB
    ( DBFactory (..)
    , DBLayer (..)
    , ErrNoSuchWallet (..)
    , PrimaryKey (..)
    , cleanDB
    )
import Cardano.Wallet.DB.Arbitrary
    ( KeyValPairs (..) )
import Cardano.Wallet.DB.Properties
    ( properties )
import Cardano.Wallet.DB.Sqlite
    ( DefaultFieldValues (..)
    , PersistState
    , WalletDBLog (..)
    , newDBFactory
    , newDBLayer
    , newDBLayerInMemory
    , withDBLayer
    )
import Cardano.Wallet.DB.StateMachine
    ( prop_parallel, prop_sequential, validateGenerators )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( block0, dummyGenesisParameters, dummyTimeInterpreter, mockHash )
import Cardano.Wallet.Gen
    ( genMnemonic )
import Cardano.Wallet.Logging
    ( trMessageText )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PaymentAddress (..)
    , PersistPrivateKey
    , WalletKey
    , encryptPassphrase
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..), generateKeyFromSeed )
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
import Cardano.Wallet.Primitive.Model
    ( FilteredBlock (..)
    , Wallet
    , applyBlock
    , availableBalance
    , currentTip
    , getState
    , initWallet
    )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , Block (..)
    , BlockHeader (..)
    , GenesisParameters (..)
    , PassphraseScheme (..)
    , Range
    , SlotNo (..)
    , SortOrder (..)
    , StartTime (..)
    , WalletDelegation (..)
    , WalletDelegationStatus (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , wholeRange
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..)
    , TransactionInfo (..)
    , Tx (..)
    , TxIn (..)
    , TxMeta (TxMeta, amount, direction)
    , TxOut (..)
    , TxStatus (..)
    , toTxHistory
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex, unsafeRunExceptT )
import Control.Monad
    ( forM_, forever, replicateM_, unless, void )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, mapExceptT )
import Crypto.Hash
    ( hash )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.Maybe
    ( fromMaybe, isJust, isNothing, mapMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( fromText, toText )
import Data.Time.Clock
    ( getCurrentTime )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Data.Word
    ( Word64 )
import Database.Persist.Sql
    ( DBName (..), PersistEntity (..), fieldDB )
import Numeric.Natural
    ( Natural )
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
    , xit
    )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Property, generate, property, (==>) )
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
    ( TVar, newTVarIO, readTVarIO, writeTVar )
import UnliftIO.Temporary
    ( withSystemTempDirectory, withSystemTempFile )

import qualified Cardano.Wallet.DB.Sqlite.TH as DB
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Seq
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified UnliftIO.STM as STM

spec :: Spec
spec = parallel $ do
    sqliteSpecSeq
    sqliteSpecRnd
    loggingSpec
    fileModeSpec
    describe "Manual migrations" $ do
        it "'migrate' db with no passphrase scheme set."
            testMigrationPassphraseScheme

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
            testMigrationRole @ShelleyKey
                "shelleyRole-v2020-10-13.sqlite"

        it "'migrate' db with partially applied checkpoint migration" $
            testMigrationRole @ShelleyKey
                "shelleyRole-corrupted-v2020-10-13.sqlite"

        it "'migrate' db with unused protocol parameters in checkpoints" $
            testMigrationCleanupCheckpoints @ShelleyKey
                "shelleyDerivationPrefix-v2020-10-07.sqlite"
                (GenesisParameters
                    { getGenesisBlockHash = Hash $ unsafeFromHex
                        "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"
                    , getGenesisBlockDate =
                        StartTime $ posixSecondsToUTCTime 1506203091
                    }
                )
                (BlockHeader
                    { slotNo = SlotNo 1125119
                    , blockHeight = Quantity 1124949
                    , headerHash = Hash $ unsafeFromHex
                        "3b309f1ca388459f0ce2c4ccca20ea646b75e6fc1447be032a41d43f209ecb50"
                    , parentHeaderHash = Hash $ unsafeFromHex
                        "e9414e08d8c5ca177dd0cb6a9e4bf868e1ea03389c31f5f7a6b099a3bcdfdedf"
                    }
                )

        it "'migrate' db to add fees to transactions" $
            testMigrationTxMetaFee @ShelleyKey
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
                  , Coin 174257
                  )

                -- This one (chosen because of its very round fee) has:
                --
                -- - two inputs of 1000000 each
                -- - one output of 1000000
                --
                -- which gives a delta (and fee) of 1000000

                , ( Hash $ unsafeFromHex "8f79e7f79ddeb7a7494121259832c0180c1b6bb746d8b2337cd1f4fb5b0d8216"
                  , Coin 1000000
                  )

                -- This one (chosen for its withdrawal) has:
                --
                -- - one input of 909199523
                -- - one withdrawal of 863644
                -- - two outputs of 1000000 and 908888778
                --
                -- which gives a delta (and fee) of 174389

                , ( Hash $ unsafeFromHex "eefa06dfa8ce91237117f9b4bdc4f6970c31de54906313b545dafb7ca6235171"
                  , Coin 174389
                  )

                -- This one (chosen for its high fee) has:
                --
                -- - one input of 997825743
                -- - two outputs of 1000000 and 995950742
                --
                -- which gives a delta (and fee) of 875001
                , ( Hash $ unsafeFromHex "8943f9fa4b56b32cd44ab9c22d46693882f0bbca1bc3f0705124e75c2e40b9c2"
                  , Coin 875001
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
                  , Coin 255613
                  )

                -- This one (chosen for its high ratio input:output) has:
                --
                -- - 1 input of 1000000000
                -- - 33 relatively small outputs
                -- - 1 withdrawal of 561120
                --
                -- which gives a delta (and fee) of 267537
                , ( Hash $ unsafeFromHex "15940a7c1df8696279282046ebdb1ee890d4e9ac3c5d7213f360921648b36666"
                  , Coin 267537
                  )
                ]

sqliteSpecSeq :: Spec
sqliteSpecSeq = do
    validateGenerators @(SeqState 'Mainnet ShelleyKey)
    around (withShelleyDBLayer Nothing) $ do
        parallel $ describe "Sqlite" properties
        parallel $ describe "Sqlite State machine tests" $ do
            it "Sequential" (prop_sequential :: TestDBSeq -> Property)
            xit "Parallel" prop_parallel

sqliteSpecRnd :: Spec
sqliteSpecRnd = do
    validateGenerators @(RndState 'Mainnet)
    around (withByronDBLayer Nothing) $ do
        parallel $ describe "Sqlite State machine (RndState)" $ do
            it "Sequential state machine tests"
                (prop_sequential :: TestDBRnd -> Property)

testMigrationTxMetaFee
    :: forall k s.
        ( s ~ SeqState 'Mainnet k
        , k ~ ShelleyKey
        , WalletKey k
        , PersistState s
        , PersistPrivateKey (k 'RootK)
        , PaymentAddress 'Mainnet k
        )
    => String
    -> Int
    -> [(Hash "Tx", Coin)]
    -> IO ()
testMigrationTxMetaFee dbName expectedLength caseByCase = do
    let orig = $(getTestData) </> dbName
    withSystemTempDirectory "migration-db" $ \dir -> do
        let path = dir </> "db.sqlite"
        let ti = dummyTimeInterpreter
        copyFile orig path
        (logs, result) <- captureLogging $ \tr -> do
            withDBLayer @s @k tr defaultFieldValues path ti
                $ \DBLayer{..} -> atomically
                $ do
                    [wid] <- listWallets
                    readTxHistory wid Nothing Descending wholeRange Nothing

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
        in fieldName field == unDBName fieldInDB

matchMsgManualMigration :: (DBField -> Bool) -> WalletDBLog -> Bool
matchMsgManualMigration p = \case
    MsgDB (MsgManualMigrationNeeded field _) -> p field
    _ -> False

testMigrationCleanupCheckpoints
    :: forall k s.
        ( s ~ SeqState 'Mainnet k
        , k ~ ShelleyKey
        , WalletKey k
        , PersistState s
        , PersistPrivateKey (k 'RootK)
        , PaymentAddress 'Mainnet k
        )
    => String
    -> GenesisParameters
    -> BlockHeader
    -> IO ()
testMigrationCleanupCheckpoints dbName genesisParameters tip = do
    let orig = $(getTestData) </> dbName
    withSystemTempDirectory "migration-db" $ \dir -> do
        let path = dir </> "db.sqlite"
        let ti = dummyTimeInterpreter
        copyFile orig path
        (logs, result) <- captureLogging $ \tr -> do
            withDBLayer @s @k tr defaultFieldValues path ti
                $ \DBLayer{..} -> atomically
                $ do
                    [wid] <- listWallets
                    (,) <$> readGenesisParameters wid <*> readCheckpoint wid

        length (filter (isMsgManualMigration fieldGenesisHash) logs) `shouldBe` 1
        length (filter (isMsgManualMigration fieldGenesisStart) logs) `shouldBe` 1

        (fst result) `shouldBe` Just genesisParameters
        (currentTip <$> snd result) `shouldBe` Just tip
  where
    fieldGenesisHash = fieldDB $ persistFieldDef DB.WalGenesisHash
    fieldGenesisStart = fieldDB $ persistFieldDef DB.WalGenesisStart

    isMsgManualMigration :: DBName -> WalletDBLog -> Bool
    isMsgManualMigration fieldInDB = matchMsgManualMigration $ \field ->
        fieldName field == unDBName fieldInDB

testMigrationRole
    :: forall k s.
        ( s ~ SeqState 'Mainnet k
        , WalletKey k
        , PersistState s
        , PersistPrivateKey (k 'RootK)
        , PaymentAddress 'Mainnet k
        )
    => String
    -> IO ()
testMigrationRole dbName = do
    let orig = $(getTestData) </> dbName
    withSystemTempDirectory "migration-db" $ \dir -> do
        let path = dir </> "db.sqlite"
        let ti = dummyTimeInterpreter
        copyFile orig path
        (logs, Just cp) <- captureLogging $ \tr -> do
            withDBLayer @s @k tr defaultFieldValues path ti
                $ \DBLayer{..} -> atomically
                $ do
                    [wid] <- listWallets
                    readCheckpoint wid
        let migrationMsg = filter isMsgManualMigration logs
        length migrationMsg `shouldBe` 3
        length (knownAddresses $ getState cp) `shouldBe` 71
  where
    isMsgManualMigration :: WalletDBLog -> Bool
    isMsgManualMigration = matchMsgManualMigration $ \field ->
        let fieldInDB = fieldDB $ persistFieldDef DB.SeqStateAddressRole
        in fieldName field == unDBName fieldInDB

testMigrationSeqStateDerivationPrefix
    :: forall k s.
        ( s ~ SeqState 'Mainnet k
        , WalletKey k
        , PersistState s
        , PersistPrivateKey (k 'RootK)
        )
    => String
    -> ( Index 'Hardened 'PurposeK
       , Index 'Hardened 'CoinTypeK
       , Index 'Hardened 'AccountK
       )
    -> IO ()
testMigrationSeqStateDerivationPrefix dbName prefix = do
    let orig = $(getTestData) </> dbName
    withSystemTempDirectory "migration-db" $ \dir -> do
        let path = dir </> "db.sqlite"
        let ti = dummyTimeInterpreter
        copyFile orig path
        (logs, Just cp) <- captureLogging $ \tr -> do
            withDBLayer @s @k tr defaultFieldValues path ti
                $ \DBLayer{..} -> atomically
                $ do
                    [wid] <- listWallets
                    readCheckpoint wid
        let migrationMsg = filter isMsgManualMigration logs
        length migrationMsg `shouldBe` 1
        derivationPrefix (getState cp) `shouldBe` DerivationPrefix prefix
  where
    isMsgManualMigration = matchMsgManualMigration $ \field ->
        let fieldInDB = fieldDB $ persistFieldDef DB.SeqStateDerivationPrefix
        in fieldName field == unDBName fieldInDB

testMigrationPassphraseScheme
    :: forall s k. (k ~ ShelleyKey, s ~ SeqState 'Mainnet k)
    => IO ()
testMigrationPassphraseScheme = do
    let orig = $(getTestData) </> "passphraseScheme-v2020-03-16.sqlite"
    withSystemTempDirectory "migration-db" $ \dir -> do
        let path = dir </> "db.sqlite"
        let ti = dummyTimeInterpreter
        copyFile orig path
        (logs, (a,b,c,d)) <- captureLogging $ \tr -> do
            withDBLayer @s @k tr defaultFieldValues path ti
                $ \DBLayer{..} -> atomically
                $ do
                    Just a <- readWalletMeta $ PrimaryKey walNeedMigration
                    Just b <- readWalletMeta $ PrimaryKey walNewScheme
                    Just c <- readWalletMeta $ PrimaryKey walOldScheme
                    Just d <- readWalletMeta $ PrimaryKey walNoPassphrase
                    pure (a,b,c,d)

        -- Migration is visible from the logs
        let migrationMsg = filter isMsgManualMigration logs
        length migrationMsg `shouldBe` 1

        -- The first wallet is stored in the database with only a
        -- 'passphraseLastUpdatedAt' field, but no 'passphraseScheme'. So,
        -- after the migration, both should now be `Just`.
        (passphraseScheme <$> passphraseInfo a) `shouldBe` Just EncryptWithPBKDF2

        -- The second wallet was just fine and already has a passphrase
        -- scheme set to use PBKDF2. Nothing should have changed.
        (passphraseScheme <$> passphraseInfo b) `shouldBe` Just EncryptWithPBKDF2

        -- The third wallet had a scheme too, but was using the legacy
        -- scheme. Nothing should have changed.
        (passphraseScheme <$> passphraseInfo c) `shouldBe` Just EncryptWithScrypt

        -- The last wallet had no passphrase whatsoever (restored from
        -- account public key), so it should still have NO scheme.
        (passphraseScheme <$> passphraseInfo d) `shouldBe` Nothing
  where
    isMsgManualMigration = matchMsgManualMigration $ \field ->
        let fieldInDB = fieldDB $ persistFieldDef DB.WalPassphraseScheme
        in  fieldName field == unDBName fieldInDB

    -- Coming from __test/data/passphraseScheme-v2020-03-16.sqlite__:
    --
    --     sqlite3> SELECT wallet_id, passphrase_last_updated_at,passphrase_scheme FROM wallet;
    --
    --     wallet_id                                | passphrase_last_updated_at    | passphrase_scheme
    --     =========================================+===============================+==================
    --     64581f7393190aed462fc3180ce52c3c1fe580a9 | 2020-06-02T14:22:17.48678659  |
    --     5e481f55084afda69fc9cd3863ced80fa83734aa | 2020-06-02T14:22:03.234087113 | EncryptWithPBKDF2
    --     4a6279cd71d5993a288b2c5879daa7c42cebb73d | 2020-06-02T14:21:45.418841818 | EncryptWithScrypt
    --     ba74a7d2c1157ea7f32a93f255dac30e9ebca62b |                               |
    --
    Right walNeedMigration = fromText "64581f7393190aed462fc3180ce52c3c1fe580a9"
    Right walNewScheme     = fromText "5e481f55084afda69fc9cd3863ced80fa83734aa"
    Right walOldScheme     = fromText "4a6279cd71d5993a288b2c5879daa7c42cebb73d"
    Right walNoPassphrase  = fromText "ba74a7d2c1157ea7f32a93f255dac30e9ebca62b"

{-------------------------------------------------------------------------------
                                Logging Spec
-------------------------------------------------------------------------------}

loggingSpec :: Spec
loggingSpec = withLoggingDB @(SeqState 'Mainnet ShelleyKey) $ do
    describe "Sqlite query logging" $ do
        it "should log queries at DEBUG level" $ \(getLogs, DBLayer{..}) -> do
            atomically $ unsafeRunExceptT $
                initializeWallet testPk testCpSeq testMetadata mempty gp
            logs <- getLogs
            logs `shouldHaveMsgQuery` "INSERT"

        it "should not log query parameters" $ \(getLogs, DBLayer{..}) -> do
            atomically $ unsafeRunExceptT $
                initializeWallet testPk testCpSeq testMetadata mempty gp
            let walletName = T.unpack $ coerce $ name testMetadata
            msgs <- T.unlines . mapMaybe getMsgQuery <$> getLogs
            T.unpack msgs `shouldNotContain` walletName

    describe "Sqlite observables" $ do
        it "should measure query timings" $ \(getLogs, DBLayer{..}) -> do
            let count = 5
            replicateM_ count (atomically listWallets)
            msgs <- findObserveDiffs <$> getLogs
            length msgs `shouldBe` count * 2

withLoggingDB
    :: PersistState s
    => SpecWith (IO [DBLog], DBLayer IO s ShelleyKey)
    -> Spec
withLoggingDB = around f . beforeWith clean
  where
    f act = do
        logVar <- newTVarIO []
        withDBLayer
            (traceInTVarIO logVar)
            defaultFieldValues
            Nothing
            dummyTimeInterpreter
            (\(_, db) -> act (logVar, db))
    clean (logs, db) = do
        cleanDB db
        STM.atomically $ writeTVar logs []
        pure (readTVarIO logs, db)

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
type TestDBRnd = DBLayer IO (RndState 'Mainnet) ByronKey

fileModeSpec :: Spec
fileModeSpec =  do
    describe "Check db opening/closing" $ do
        it "Opening and closing of db works" $ do
            replicateM_ 25 $ do
                db <- Just <$> temporaryDBFile
                withShelleyDBLayer @(SeqState 'Mainnet ShelleyKey) db
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
                            liftIO $ threadDelay 10000
                            void $ readCheckpoint $ PrimaryKey testWid

                killThread pid *> removeDatabase testWid
                listDirectory dir `shouldReturn` mempty

        it "removeDatabase still works if file is opened" $ do
            withDBFactory $ \dir DBFactory{..} -> do
                -- set up a database file
                withDatabase testWid $ \(DBLayer{..} :: TestDBSeq) -> pure ()
                files <- listDirectory dir
                files `shouldNotBe` mempty

                -- Try removing the database when it's already opened for
                -- reading for 100ms.
                -- This simulates an antivirus program on windows which may
                -- interfere with file deletion.
                whileFileOpened 100000 (dir </> head files) (removeDatabase testWid)
                listDirectory dir `shouldReturn` mempty

        it "removeDatabase waits for connections to close" $ do
            withDBFactory $ \_ DBFactory{..} -> do
                closed <- newEmptyMVar

                let conn =
                        withDatabase testWid $ \(DBLayer{..} :: TestDBSeq) -> do
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
                    initializeWallet testPk testCpSeq testMetadata mempty gp
                atomically listWallets `shouldReturn` [testPk]
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
            withShelleyDBLayer f $ \DBLayer{..} -> do
                atomically $ unsafeRunExceptT $
                    initializeWallet testPk testCp testMetadata mempty gp
            testOpeningCleaning f listWallets' [testPk] []

        it "create and get meta works" $ \f -> do
            meta <- withShelleyDBLayer f $ \DBLayer{..} -> do
                now <- getCurrentTime
                let meta = testMetadata
                       { passphraseInfo = Just $ WalletPassphraseInfo now EncryptWithPBKDF2 }
                atomically $ unsafeRunExceptT $
                    initializeWallet testPk testCp meta mempty gp
                return meta
            testOpeningCleaning f (`readWalletMeta'` testPk) (Just meta) Nothing

        it "create and get private key" $ \f -> do
            (k, h) <- withShelleyDBLayer f $ \db@DBLayer{..} -> do
                atomically $ unsafeRunExceptT $
                    initializeWallet testPk testCp testMetadata mempty gp
                unsafeRunExceptT $ attachPrivateKey db testPk
            testOpeningCleaning f (`readPrivateKey'` testPk) (Just (k, h)) Nothing

        it "put and read tx history (Ascending)" $ \f -> do
            withShelleyDBLayer f $ \DBLayer{..} -> do
                atomically $ do
                    unsafeRunExceptT $
                        initializeWallet testPk testCp testMetadata mempty gp
                    unsafeRunExceptT $ putTxHistory testPk testTxs
            testOpeningCleaning
                f
                (\db' -> readTxHistory' db' testPk Ascending wholeRange Nothing)
                testTxs
                mempty

        it "put and read tx history (Decending)" $ \f -> do
            withShelleyDBLayer f $ \DBLayer{..} -> do
                atomically $ do
                    unsafeRunExceptT $
                        initializeWallet testPk testCp testMetadata mempty gp
                    unsafeRunExceptT $ putTxHistory testPk testTxs
            testOpeningCleaning
                f
                (\db' -> readTxHistory' db' testPk Descending wholeRange Nothing)
                testTxs
                mempty

        it "put and read checkpoint" $ \f -> do
            withShelleyDBLayer f $ \DBLayer{..} -> do
                atomically $ do
                    unsafeRunExceptT $
                        initializeWallet testPk testCp testMetadata mempty gp
                    unsafeRunExceptT $ putCheckpoint testPk testCp
            testOpeningCleaning f (`readCheckpoint'` testPk) (Just testCp) Nothing

        describe "Golden rollback scenarios" $ do
            let dummyHash x = Hash $ x <> BS.pack (replicate (32 - (BS.length x)) 0)
            let dummyAddr x = Address $ x <> BS.pack (replicate (32 - (BS.length x)) 0)

            it "(Regression test #1575) - TxMetas and checkpoints should \
               \rollback to the same place" $ \f -> do
              withShelleyDBLayer f $ \db@DBLayer{..} -> do

                let ourAddrs = knownAddresses (getState testCp)

                atomically $ unsafeRunExceptT $ initializeWallet
                    testPk testCp testMetadata mempty gp

                let mockApply h mockTxs = do
                        Just cpA <- atomically $ readCheckpoint testPk
                        let slotA = slotNo $ currentTip cpA
                        let Quantity bhA = blockHeight $ currentTip cpA
                        let hashA = headerHash $ currentTip cpA
                        let fakeBlock = Block
                                (BlockHeader
                                { slotNo = slotA + 100
                                -- Increment blockHeight by steps greater than k
                                -- Such that old checkpoints are always pruned.
                                , blockHeight = Quantity $ bhA + 5000
                                , headerHash = h
                                , parentHeaderHash = hashA
                                })
                                mockTxs
                                mempty
                        let (FilteredBlock _ txs, cpB) = applyBlock fakeBlock cpA
                        atomically $ do
                            unsafeRunExceptT $ putCheckpoint testPk cpB
                            unsafeRunExceptT $ putTxHistory testPk txs
                            unsafeRunExceptT $ prune testPk (Quantity 2160)

                let mockApplyBlock1 = mockApply (dummyHash "block1")
                        [ Tx (dummyHash "tx1")
                            Nothing
                            [(TxIn (dummyHash "faucet") 0, Coin 4)]
                            [TxOut (fst $ head ourAddrs) (coinToBundle 4)]
                            mempty
                            Nothing
                        ]

                -- Slot 1 0
                mockApplyBlock1
                getAvailableBalance db `shouldReturn` 4

                -- Slot 200
                mockApply (dummyHash "block2a")
                    [ Tx (dummyHash "tx2a")
                        Nothing
                        [ (TxIn (dummyHash "tx1") 0, Coin 4) ]
                        [ TxOut (dummyAddr "faucetAddr2") (coinToBundle 2)
                        , TxOut (fst $ ourAddrs !! 1) (coinToBundle 2)
                        ]
                        mempty
                        Nothing
                    ]

                -- Slot 300
                mockApply (dummyHash "block3a") []
                getAvailableBalance db `shouldReturn` 2
                getTxsInLedger db `shouldReturn` [(Outgoing, 2), (Incoming, 4)]

                atomically . void . unsafeRunExceptT $
                    rollbackTo testPk (SlotNo 200)
                Just cp <- atomically $ readCheckpoint testPk
                slotNo (currentTip cp) `shouldBe` (SlotNo 0)

                getTxsInLedger db `shouldReturn` []

    describe "random operation chunks property" $ do
        it "realize a random batch of operations upon one db open"
            (property $ prop_randomOpChunks @(SeqState 'Mainnet ShelleyKey))

-- This property checks that executing series of wallet operations in a single
-- SQLite session has the same effect as executing the same operations over
-- multiple sessions.
prop_randomOpChunks
    :: (Eq s, PersistState s, Show s)
    => KeyValPairs (PrimaryKey WalletId) (Wallet s, WalletMetadata)
    -> Property
prop_randomOpChunks (KeyValPairs pairs) =
    not (null pairs) ==> monadicIO (liftIO prop)
  where
    prop = do
        filepath <- temporaryDBFile
        withShelleyDBLayer filepath $ \dbF -> do
            cleanDB dbF
            withShelleyDBLayerInMemory $ \dbM -> do
                cleanDB dbM
                forM_ pairs (insertPair dbM)
                cutRandomly pairs >>= mapM_ (mapM (insertPair dbF))
                dbF `shouldBeConsistentWith` dbM

    insertPair
        :: DBLayer IO s k
        -> (PrimaryKey WalletId, (Wallet s, WalletMetadata))
        -> IO ()
    insertPair DBLayer{..} (k, (cp, meta)) = do
        keys <- atomically listWallets
        if k `elem` keys then atomically $ do
            unsafeRunExceptT $ putCheckpoint k cp
            unsafeRunExceptT $ putWalletMeta k meta
        else do
            atomically $ unsafeRunExceptT $ initializeWallet k cp meta mempty gp
            Set.fromList <$> atomically listWallets
                `shouldReturn` Set.fromList (k:keys)

    shouldBeConsistentWith :: (Eq s, Show s) => DBLayer IO s k -> DBLayer IO s k -> IO ()
    shouldBeConsistentWith db1@DBLayer{..} db2 = do
        wids1 <- Set.fromList <$> listWallets' db1
        wids2 <- Set.fromList <$> listWallets' db2
        wids1 `shouldBe` wids2

        forM_ wids1 $ \walId -> do
            cps1 <- readCheckpoint' db1 walId
            cps2 <- readCheckpoint' db2 walId
            cps1 `shouldBe` cps2

        forM_ wids1 $ \walId -> do
            meta1 <- readWalletMeta' db1 walId
            meta2 <- readWalletMeta' db2 walId
            meta1 `shouldBe` meta2

-- | Test that data is preserved between open / close of the same database and
-- that cleaning up happens as expected.
testOpeningCleaning
    :: (Show s, Eq s)
    => FilePath
    -> (DBLayer IO (SeqState 'Mainnet ShelleyKey) ShelleyKey -> IO s)
    -> s
    -> s
    -> Expectation
testOpeningCleaning filepath call expectedAfterOpen expectedAfterClean = do
    withShelleyDBLayer filepath $ \db -> do
        call db `shouldReturn` expectedAfterOpen
        _ <- cleanDB db
        call db `shouldReturn` expectedAfterClean
    withShelleyDBLayer filepath $ \db -> do
        call db `shouldReturn` expectedAfterClean

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
            defaultFieldValues
            fp
            ti
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

-- Note: Having two separate helpers with concrete key types reduces the need
-- for type-application everywhere.
withByronDBLayer
    :: PersistState s
    => Maybe FilePath -- ^ Just for on-disk db, Nothing for in-memory.
    -> ((DBLayer IO s ByronKey) -> IO a)
    -> IO a
withByronDBLayer fp = withDBLayer
    nullTracer
    defaultFieldValues
    fp
    dummyTimeInterpreter

withShelleyDBLayer
    :: PersistState s
    => FilePath
    -> (DBLayer IO s ShelleyKey -> IO a)
    -> IO a
withShelleyDBLayer fp = withDBLayer
    nullTracer  -- fixme: capture logging
    defaultFieldValues
    fp
    dummyTimeInterpreter

listWallets'
    :: DBLayer m s k
    -> m [PrimaryKey WalletId]
listWallets' DBLayer{..} =
    atomically listWallets

readCheckpoint'
    :: DBLayer m s k
    -> PrimaryKey WalletId
    -> m (Maybe (Wallet s))
readCheckpoint' DBLayer{..} =
    atomically . readCheckpoint

readWalletMeta'
    :: DBLayer m s k
    -> PrimaryKey WalletId
    -> m (Maybe WalletMetadata)
readWalletMeta' DBLayer{..} =
    atomically . readWalletMeta

readTxHistory'
    :: DBLayer m s k
    -> PrimaryKey WalletId
    -> SortOrder
    -> Range SlotNo
    -> Maybe TxStatus
    -> m [(Tx, TxMeta)]
readTxHistory' DBLayer{..} a0 a1 a2 =
    atomically . fmap (fmap toTxHistory) . readTxHistory a0 Nothing a1 a2

readPrivateKey'
    :: DBLayer m s k
    -> PrimaryKey WalletId
    -> m (Maybe (k 'RootK XPrv, Hash "encryption"))
readPrivateKey' DBLayer{..} =
    atomically . readPrivateKey

-- | Attach an arbitrary private key to a wallet
attachPrivateKey
    :: DBLayer IO s ShelleyKey
    -> PrimaryKey WalletId
    -> ExceptT ErrNoSuchWallet IO (ShelleyKey 'RootK XPrv, Hash "encryption")
attachPrivateKey DBLayer{..} wid = do
    let pwd = Passphrase $ BA.convert $ T.encodeUtf8 "simplevalidphrase"
    seed <- liftIO $ generate $ SomeMnemonic <$> genMnemonic @15
    let k = generateKeyFromSeed (seed, Nothing) pwd
    h <- liftIO $ encryptPassphrase pwd
    mapExceptT atomically $ putPrivateKey wid (k, h)
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
                                   Test data
-------------------------------------------------------------------------------}

coinToBundle :: Word64 -> TokenBundle
coinToBundle = TokenBundle.fromCoin . Coin

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
    , delegation = WalletDelegation NotDelegating []
    }

testWid :: WalletId
testWid = WalletId (hash ("test" :: ByteString))

testPk :: PrimaryKey WalletId
testPk = PrimaryKey testWid

testTxs :: [(Tx, TxMeta)]
testTxs =
    [ ( Tx (mockHash @String "tx2")
        Nothing
        [ (TxIn (mockHash @String "tx1") 0, Coin 1)]
        [ TxOut (Address "addr") (coinToBundle 1) ]
        mempty
        Nothing
      , TxMeta
        InLedger Incoming (SlotNo 140) (Quantity 0) (Coin 1337144) Nothing
      )
    ]

gp :: GenesisParameters
gp = dummyGenesisParameters

{-------------------------------------------------------------------------------
                    Helpers for golden rollback tests
-------------------------------------------------------------------------------}

getAvailableBalance :: DBLayer IO s k -> IO Natural
getAvailableBalance DBLayer{..} = do
    cp <- fmap (fromMaybe (error "nothing")) <$> atomically $ readCheckpoint testPk
    pend <- atomically $ fmap toTxHistory
        <$> readTxHistory testPk Nothing Descending wholeRange (Just Pending)
    return $ fromIntegral $ unCoin $ TokenBundle.getCoin $
        availableBalance (Set.fromList $ map fst pend) cp

getTxsInLedger :: DBLayer IO s k -> IO ([(Direction, Natural)])
getTxsInLedger DBLayer {..} = do
    pend <- atomically $ fmap toTxHistory
        <$> readTxHistory testPk Nothing Descending wholeRange (Just InLedger)
    return $ map (\(_, m) -> (direction m, fromIntegral $ unCoin $ amount m)) pend

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

{-------------------------------------------------------------------------------
                      Test data and instances - Random AD
-------------------------------------------------------------------------------}

instance Eq (RndState t) where
    (==)
        (RndState _ idx1 addrs1 pending1 gen1)
        (RndState _ idx2 addrs2 pending2 gen2) =
           idx1 == idx2
        && addrs1 == addrs2
        && pending1 == pending2
        && show gen1 == show gen2
