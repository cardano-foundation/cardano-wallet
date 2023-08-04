{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  ( TxId (..)
  )
import Cardano.Wallet.DB.Store.Submissions.Operations
  ( SubmissionMeta (..)
  , mkStoreSubmissions
  )
import Cardano.Wallet.Primitive.Types
  ( SlotNo (..)
  )
import Cardano.Wallet.Primitive.Types.Coin
  ( Coin (Coin)
  )
import Cardano.Wallet.Primitive.Types.Tx
  ( SealedTx (..)
  , mockSealedTx
  )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
  ( Direction (Outgoing)
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
import Data.ByteString qualified as BS
import Data.Quantity
  ( Quantity (..)
  )
import System.Random
  ( Random
  )
import Test.Hspec
  ( Spec
  , around
  , describe
  , it
  )
import Test.QuickCheck
  ( Arbitrary (..)
  , property
  )
import Test.Store
  ( prop_StoreUpdate
  )
import Prelude

spec :: Spec
spec = do
  around (withDBInMemory ForeignKeysDisabled) $ do
    describe "submissions via API for a single wallet store" $ do
      it "respects store laws"
        $ property . prop_SingleWalletStoreLawsOperations

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
