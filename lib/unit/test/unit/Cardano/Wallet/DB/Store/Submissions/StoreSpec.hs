{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.DB.Store.Submissions.StoreSpec (spec) where

import Cardano.DB.Sqlite
    ( ForeignKeysSetting (..)
    , runQuery
    )
import Cardano.Wallet.DB.Arbitrary
    (
    )
import Cardano.Wallet.DB.Fixtures
    ( WalletProperty
    , initializeWalletTable
    , logScale
    , withDBInMemory
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( DappSubmissionInputRole (..)
    , DappSubmissionStatusEnum (..)
    , TxId (..)
    )
import Cardano.Wallet.DB.Store.Submissions.Operations
    ( DurableSubmission (..)
    , DurableSubmissionInput (..)
    , DurableSubmissionInsert (..)
    , SubmissionMeta (..)
    , claimDurableSubmissionAttempt
    , isRollbackConflict
    , insertOrClassifyDurableSubmission
    , mkStoreSubmissions
    , readDurableSubmissions
    , updateDurableSubmission
    )
import Cardano.Wallet.Primitive.Types
    ( SlotNo (..)
    , WalletId (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (Coin)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..)
    , mockSealedTx
    )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( Direction (Outgoing)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Submissions.OperationsSpec
    ( genOperationsDelta
    )
import Cardano.Wallet.Submissions.Submissions
    ( Submissions (..)
    )
import Control.Monad
    ( replicateM
    )
import Cryptography.Hash.Core
    ( hash
    )
import Data.List
    ( find
    )
import Data.Quantity
    ( Quantity (..)
    )
import Database.Persist.Sql
    ( rawExecute
    )
import System.Random
    ( Random
    )
import Test.Data.Store
    ( prop_StoreUpdate
    )
import Test.Hspec
    ( Spec
    , around
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , property
    )
import Prelude
import qualified Data.ByteString as BS


spec :: Spec
spec = do
    around (withDBInMemory ForeignKeysDisabled) $ do
        describe "submissions via API for a single wallet store" $ do
            it "respects store laws"
                $ property . prop_SingleWalletStoreLawsOperations
            it
                "locks shared inputs once while preserving replay and conflict handling"
                $ \db -> do
                    let wid = WalletId $ hash @BS.ByteString "submission-test-wallet"
                        tx1 = TxId $ Hash $ BS.replicate 32 1
                        tx2 = TxId $ Hash $ BS.replicate 32 2
                        source = TxId $ Hash $ BS.replicate 32 3
                        claim = DurableSubmissionInput source 0 NormalInputE
                        collateral = DurableSubmissionInput source 0 CollateralInputE
                        shared = [claim, collateral]
                        submission tx sealed =
                            DurableSubmission
                                wid
                                tx
                                (mockSealedTx sealed)
                                Nothing
                                True
                                AuthorizedE
                                0
                                Nothing
                                Nothing
                                Nothing
                                Nothing
                    outcomes <- runQuery db $ do
                        initializeWalletTable wid
                        rawExecute
                            "CREATE UNIQUE INDEX IF NOT EXISTS dapp_submission_claim ON dapp_submission_input \
                            \(wallet_id, source_tx_id, source_index) WHERE active = 1"
                            []
                        invalid <-
                            insertOrClassifyDurableSubmission
                                (submission tx1 "one")
                                [claim, claim]
                        empty <- length <$> readDurableSubmissions wid
                        first <-
                            insertOrClassifyDurableSubmission (submission tx1 "one") shared
                        replay <-
                            insertOrClassifyDurableSubmission
                                (submission tx1 "one")
                                (reverse shared)
                        conflict <-
                            insertOrClassifyDurableSubmission (submission tx2 "two") [collateral]
                        updateDurableSubmission
                            (submission tx1 "one")
                                { durableAuthorized = False
                                , durableStatus = RejectedE
                                }
                        released <-
                            insertOrClassifyDurableSubmission (submission tx2 "two") [claim]
                        rows <- readDurableSubmissions wid
                        pure (invalid, empty, first, replay, conflict, released, length rows)
                    outcomes
                        `shouldBe` ( DurableSubmissionInputConflict
                                   , 0
                                   , DurableSubmissionAuthorized
                                   , DurableSubmissionReplay (submission tx1 "one")
                                   , DurableSubmissionInputConflict
                                   , DurableSubmissionAuthorized
                                   , 2
                                   )
            it "allows exactly one durable broadcast owner" $ \db -> do
                let wid = WalletId $ hash @BS.ByteString "submission-attempt-owner"
                    tx = TxId $ Hash $ BS.replicate 32 1
                    source = TxId $ Hash $ BS.replicate 32 2
                    claim = DurableSubmissionInput source 0 NormalInputE
                    submission =
                        DurableSubmission
                            wid
                            tx
                            (mockSealedTx "one")
                            Nothing
                            True
                            AuthorizedE
                            0
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                owners <- runQuery db $ do
                    initializeWalletTable wid
                    _ <- insertOrClassifyDurableSubmission submission [claim]
                    first <-
                        claimDurableSubmissionAttempt
                            wid
                            tx
                            0
                            (read "2026-01-01 00:00:00 UTC")
                    second <-
                        claimDurableSubmissionAttempt
                            wid
                            tx
                            0
                            (read "2026-01-01 00:00:00 UTC")
                    pure (durableStatus <$> first, durableStatus <$> second)
                owners `shouldBe` (Just BroadcastingE, Nothing)
            it "retains claims after an ambiguous broadcast outcome" $ \db -> do
                let wid = WalletId $ hash @BS.ByteString "submission-unknown-outcome"
                    tx = TxId $ Hash $ BS.replicate 32 1
                    source = TxId $ Hash $ BS.replicate 32 2
                    claim = DurableSubmissionInput source 0 NormalInputE
                    submission =
                        DurableSubmission
                            wid
                            tx
                            (mockSealedTx "one")
                            Nothing
                            True
                            AuthorizedE
                            0
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                status <- runQuery db $ do
                    initializeWalletTable wid
                    _ <- insertOrClassifyDurableSubmission submission [claim]
                    Just broadcasting <-
                        claimDurableSubmissionAttempt
                            wid
                            tx
                            0
                            (read "2026-01-01 00:00:00 UTC")
                    updateDurableSubmission
                        broadcasting
                            { durableStatus = OutcomeUnknownE
                            , durableBroadcastStarted = Nothing
                            }
                    [stored] <- readDurableSubmissions wid
                    pure $ durableStatus stored
                status `shouldBe` OutcomeUnknownE

            it "restores rolled-back claims after a competing submission releases them" $ \db -> do
                let wid = WalletId $ hash @BS.ByteString "submission-rollback"
                    tx1 = TxId $ Hash $ BS.replicate 32 1
                    tx2 = TxId $ Hash $ BS.replicate 32 2
                    tx3 = TxId $ Hash $ BS.replicate 32 3
                    source = TxId $ Hash $ BS.replicate 32 4
                    claim = DurableSubmissionInput source 0 NormalInputE
                    submission tx =
                        DurableSubmission wid tx (mockSealedTx "body")
                            Nothing True AuthorizedE 0 Nothing Nothing Nothing Nothing
                outcomes <- runQuery db $ do
                    initializeWalletTable wid
                    rawExecute
                        "CREATE UNIQUE INDEX IF NOT EXISTS dapp_submission_claim ON dapp_submission_input \
                        \(wallet_id, source_tx_id, source_index) WHERE active = 1"
                        []
                    first <- insertOrClassifyDurableSubmission (submission tx1) [claim]
                    updateDurableSubmission (submission tx1)
                        { durableAuthorized = False, durableStatus = InLedgerDappE }
                    second <- insertOrClassifyDurableSubmission (submission tx2) [claim]
                    updateDurableSubmission (submission tx1)
                        { durableAuthorized = False, durableStatus = SubmittedE }
                    rows <- readDurableSubmissions wid
                    old <- maybe (fail "missing rolled-back row") pure
                        $ find ((== tx1) . durableTxId) rows
                    newer <- maybe (fail "missing competing row") pure
                        $ find ((== tx2) . durableTxId) rows
                    attempted <- claimDurableSubmissionAttempt wid tx1 0
                        (read "2026-01-01 00:00:00 UTC")
                    updateDurableSubmission newer
                        { durableAuthorized = False, durableStatus = RejectedE }
                    updateDurableSubmission old
                        { durableAuthorized = False, durableStatus = SubmittedE }
                    restoredRows <- readDurableSubmissions wid
                    restored <- maybe (fail "missing restored row") pure
                        $ find ((== tx1) . durableTxId) restoredRows
                    blocked <- insertOrClassifyDurableSubmission (submission tx3) [claim]
                    let migrated = (submission tx3)
                            { durableAuthorized = False, durableStatus = OutcomeUnknownE }
                    pure (first, second, isRollbackConflict old,
                        durableStatus old, durableAuthorized old,
                        durableStatus <$> attempted, isRollbackConflict restored,
                        isRollbackConflict migrated, blocked)
                outcomes `shouldBe`
                    ( DurableSubmissionAuthorized, DurableSubmissionAuthorized,
                      True, OutcomeUnknownE, False, Nothing, False, False,
                      DurableSubmissionInputConflict )

deriving instance Random SlotNo

dummyMetadata :: SubmissionMeta
dummyMetadata = SubmissionMeta 0 (Quantity 0) (Coin 0) Outgoing 0

prop_SingleWalletStoreLawsOperations :: WalletProperty
prop_SingleWalletStoreLawsOperations db wid =
    prop_StoreUpdate
        (runQuery db)
        setupStore
        (pure $ Submissions mempty 0 0)
        (logScale . genOperationsDelta (pure dummyMetadata))
  where
    setupStore = do
        initializeWalletTable wid
        pure $ mkStoreSubmissions wid

{-------------------------------------------------------------------------------
    Arbitrary instances
-------------------------------------------------------------------------------}
instance Arbitrary TxId where
    arbitrary = TxId <$> arbitrary

instance Arbitrary SealedTx where
    arbitrary = mockSealedTx . BS.pack <$> replicateM 16 arbitrary
