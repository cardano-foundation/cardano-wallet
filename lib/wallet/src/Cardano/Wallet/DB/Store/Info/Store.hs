{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
module Cardano.Wallet.DB.Store.Info.Store
  ( -- * Synopsis

    -- | 'Store' of wallet metadata.
    mkStoreInfo
  , DeltaWalletInfo (..)
  , WalletInfo (..)
  , StoreInfo
  )
where

import Cardano.Wallet.DB.Errors
  ( ErrWalletNotInitialized (ErrWalletNotInitialized)
  )
import Cardano.Wallet.DB.Sqlite.Schema
  ( EntityField (..)
  , Wallet (..)
  )
import Cardano.Wallet.DB.Sqlite.Types
  ( BlockId (BlockId)
  , getBlockId
  )
import Cardano.Wallet.Primitive.Passphrase.Types
  ( WalletPassphraseInfo (WalletPassphraseInfo, lastUpdatedAt, passphraseScheme)
  )
import Cardano.Wallet.Primitive.Types
  ( GenesisParameters (..)
  , StartTime (StartTime, utcTimeOfStartTime)
  , WalletId
  , WalletMetadata (..)
  , WalletName (WalletName)
  , getWalletName
  )
import Cardano.Wallet.Primitive.Types.Hash
  ( Hash (Hash)
  )
import Control.Exception
  ( SomeException (..)
  )
import Control.Lens
  ( to
  , (^.)
  )
import Control.Monad.Class.MonadThrow
  ( throwIO
  )
import Data.Delta
  ( Delta (..)
  )
import Data.Store
  ( UpdateStore
  , mkUpdateStore
  , updateLoad
  )
import Database.Persist
  ( Entity (entityVal)
  , Filter
  , PersistQueryRead (selectFirst)
  , PersistQueryWrite (updateWhere)
  , Update
  , insert_
  , (=.)
  )
import Database.Persist.Sql
  ( SqlPersistT
  , deleteWhere
  )
import Fmt
  ( Buildable (..)
  )
import GHC.Generics
  ( Generic
  )
import Prelude

mkWalletMetadataUpdate :: WalletMetadata -> [Update Wallet]
mkWalletMetadataUpdate meta =
  [ WalName =. meta ^. #name . to getWalletName
  , WalCreationTime =. meta ^. #creationTime
  , WalPassphraseLastUpdatedAt =. lastUpdatedAt <$> meta ^. #passphraseInfo
  , WalPassphraseScheme =. passphraseScheme <$> meta ^. #passphraseInfo
  ]

metadataFromEntity :: Wallet -> WalletMetadata
metadataFromEntity wal =
  WalletMetadata
    { name = WalletName (walName wal)
    , creationTime = walCreationTime wal
    , passphraseInfo =
        WalletPassphraseInfo
          <$> walPassphraseLastUpdatedAt wal
          <*> walPassphraseScheme wal
    }

-- | Compound type for 'WalletMetadata' and 'GenesisParameters'.
data WalletInfo = WalletInfo
  { walletId :: !WalletId
  , walletMeta :: !WalletMetadata
  , walletGenesisParameters :: !GenesisParameters
  }
  deriving (Eq, Show, Generic)

-- | Delta type for 'WalletInfo'. Can change only 'WalletMetadata'.
newtype DeltaWalletInfo
  = UpdateWalletMetadata WalletMetadata
  deriving (Eq, Show)

instance Buildable DeltaWalletInfo where
  build (UpdateWalletMetadata meta) = build meta

instance Delta DeltaWalletInfo where
  type Base DeltaWalletInfo = WalletInfo
  apply (UpdateWalletMetadata meta) wi = wi {walletMeta = meta}

-- | 'Store' of wallet metadata.
type StoreInfo = UpdateStore (SqlPersistT IO) DeltaWalletInfo

-- | Create a 'Store' of wallet metadata.
mkStoreInfo :: StoreInfo
mkStoreInfo = mkUpdateStore load write update
  where
    load :: SqlPersistT IO (Either SomeException WalletInfo)
    load =
      exceptNothing . fmap (fromWalletEntity . entityVal)
        <$> selectFirst [] []

    write :: WalletInfo -> SqlPersistT IO ()
    write wi = do
      deleteWhere ([] :: [Filter Wallet])
      insert_ $ toWalletEntity wi

    update :: Maybe WalletInfo -> DeltaWalletInfo -> SqlPersistT IO ()
    update = updateLoad load throwIO $ \_ -> \case
      UpdateWalletMetadata meta -> do
        updateWhere [] $ mkWalletMetadataUpdate meta

    exceptNothing :: Maybe WalletInfo -> Either SomeException WalletInfo
    exceptNothing = maybe (Left $ SomeException ErrWalletNotInitialized) Right

toWalletEntity :: WalletInfo -> Wallet
toWalletEntity (WalletInfo wid meta gp) =
  Wallet
    { walId = wid
    , walName = meta ^. #name . to getWalletName
    , walCreationTime = meta ^. #creationTime
    , walPassphraseLastUpdatedAt = lastUpdatedAt <$> meta ^. #passphraseInfo
    , walPassphraseScheme = passphraseScheme <$> meta ^. #passphraseInfo
    , walGenesisHash = BlockId $ hashConversion (gp ^. #getGenesisBlockHash)
    , walGenesisStart = utcTimeOfStartTime (gp ^. #getGenesisBlockDate)
    }

fromWalletEntity :: Wallet -> WalletInfo
fromWalletEntity wal =
  WalletInfo
    { walletId = walId wal
    , walletMeta = metadataFromEntity wal
    , walletGenesisParameters =
        GenesisParameters
          { getGenesisBlockHash =
              hashConversion
                $ getBlockId (walGenesisHash wal)
          , getGenesisBlockDate = StartTime (walGenesisStart wal)
          }
    }

hashConversion :: Hash a -> Hash b
hashConversion (Hash a) = Hash a
