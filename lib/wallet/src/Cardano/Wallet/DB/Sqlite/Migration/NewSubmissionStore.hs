{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.DB.Sqlite.Migration.NewSubmissionStore
    ( createDatabase) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( LocalTxSubmission (..)
    , TxCollateral (..)
    , TxCollateralOut (..)
    , TxCollateralOutToken (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxOutToken (TxOutToken)
    , TxWithdrawal (TxWithdrawal)
    , Wallet (..)
    , migrateAll
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( BlockId (..), TxId (..) )
import Cardano.Wallet.DB.Store.Meta.Model
    ( TxMetaHistory (TxMetaHistory) )
import Cardano.Wallet.DB.Store.Submissions.Model
    ( TxLocalSubmissionHistory (TxLocalSubmissionHistory) )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( TxRelation (TxRelation), TxSet (TxSet) )
import Cardano.Wallet.DB.Store.Wallets.Model
    ( MetasAndSubmissionsHistory, TxWalletsHistory )
import Cardano.Wallet.DB.Store.Wallets.Store
    ( mkStoreTxWalletsHistory )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx.SealedTx
    ( mockSealedTx )
import Control.Monad.Logger
    ( runStderrLoggingT )
import Control.Monad.Reader
    ( mapReaderT )
import Control.Monad.Trans
    ( MonadIO (liftIO) )
import Control.Monad.Writer
    ( MonadWriter (tell), execWriter )
import Crypto.Hash
    ( hash )
import Data.ByteString.Char8
    ( ByteString )
import Data.DBVar
    ( Store (..) )
import Data.Map.Strict
    ( Map )
import Data.Time
    ( getCurrentTime )
import Database.Persist.Sql
    ( SqlPersistT, insert_, runMigration )
import Database.Persist.Sqlite
    ( runSqlite )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxMeta as W

createDatabase :: IO ()
createDatabase = runStderrLoggingT $ runSqlite "before_submission.sqlite" $ do
    runMigration migrateAll
    liftIO mkWalletEntity >>= insert_
    mapReaderT liftIO storeAWallet

mkWalletEntity :: IO Wallet
mkWalletEntity  = do
  ct <- getCurrentTime
  pure $ Wallet
    { walId = wid "a"
    , walName = "test"
    , walCreationTime = ct
    , walPassphraseLastUpdatedAt = Nothing
    , walPassphraseScheme = Nothing
    , walGenesisHash = BlockId $ W.Hash ""
    , walGenesisStart = ct
    }

storeAWallet :: SqlPersistT IO ()
storeAWallet = writeS mkStoreTxWalletsHistory history

history :: TxWalletsHistory
history = (txSet, txMetaAndSubmissions)
  where
    txSet :: TxSet
    txSet = TxSet $
      execWriter $ do
        mkRelation "0" dumbTxRelation
        mkRelation "1" dumbTxRelation
        mkRelation "2" dumbTxRelation
    txMetaAndSubmissions :: Map W.WalletId MetasAndSubmissionsHistory
    txMetaAndSubmissions =
      [ ( wid',
          (txMetaHistory, txLocalSubmissionHistory)
        )
      ]
      where
        txMetaHistory = TxMetaHistory $
          execWriter $ do
            mkRelation "0" $ dumbMetaRelation W.InLedger
            mkRelation "1" $ dumbMetaRelation W.Pending
            mkRelation "2" $ dumbMetaRelation W.Expired
        txLocalSubmissionHistory = TxLocalSubmissionHistory $
          execWriter $ do
            mkRelation "1" $ dumbLocalSubmissionRelation wid'
            mkRelation "2" $ dumbLocalSubmissionRelation wid'
        wid' = wid "a"

mkTxId :: ByteString -> TxId
mkTxId = TxId . W.Hash

mkRelation :: MonadWriter (Map TxId r) m
  => ByteString -> (TxId -> r) -> m ()
mkRelation txId r = tell ([(txId', r txId')] )
  where txId' = mkTxId txId

wid :: ByteString -> W.WalletId
wid = W.WalletId . hash @ByteString

novalue :: W.Coin
novalue = W.Coin 0

dumbTxRelation :: TxId -> TxRelation
dumbTxRelation id' =
    TxRelation
        [TxIn id' 0 (mkTxId "1") 0 novalue]
        [TxCollateral id' 0 (mkTxId "1") 0 novalue]
        [ ( TxOut id' 0 (W.Address "addr") novalue
          , [TxOutToken id' 0
              (UnsafeTokenPolicyId (W.Hash "0"))
              (UnsafeTokenName "token")
              (TokenQuantity 42)
              ]
          )
        ]
        (Just  ( TxCollateralOut id' (W.Address "addr") novalue
          , [TxCollateralOutToken id'
              (UnsafeTokenPolicyId (W.Hash "0"))
              (UnsafeTokenName "token")
              (TokenQuantity 42)
              ]
          )
        )
        [ TxWithdrawal id' novalue ( RewardAccount "rw") ]
        Nothing

dumbMetaRelation :: W.TxStatus -> TxId -> TxMeta
dumbMetaRelation status id' =
  TxMeta
    id'
    (wid "a")
    status
    W.Outgoing
    1000
    50
    novalue
    Nothing
    Nothing
    Nothing
    Nothing

dumbLocalSubmissionRelation :: W.WalletId -> TxId -> LocalTxSubmission
dumbLocalSubmissionRelation wid' id' =
  LocalTxSubmission
    id'
    wid'
    999
    (mockSealedTx "")
