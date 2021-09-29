{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Demo.Database where

import Prelude

import Conduit
    ( ResourceT )
import Control.Monad.Class.MonadSTM
    ( MonadSTM (..) )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Logger
    ( NoLoggingT )
import Data.Chain
    ( DeltaChain (..), Edge (..), chainIntoTable )
import Data.Generics.Internal.VL
    ( Iso', iso, withIso )
import Data.Proxy
    ( Proxy (..) )
import Data.Table
    ( DeltaDB (..), Pile (..), tableIntoDatabase )
import Data.Text
    ( Text )
import Data.Word
    ( Word32 )
import Database.Persist.Delta
    ( newEntityStore, newSqlStore )
import Database.Persist.Sql
    ( SqlPersistM )
import Database.Persist.TH
    ( mkMigrate
    , mkPersist
    , mpsPrefixFields
    , persistLowerCase
    , share
    , sqlSettings
    )
import Database.Schema
    ( (:.) (..), Col (..), Primary, Table (..) )
import GHC.Generics
    ( Generic )
import Say
    ( sayShow )

import qualified Data.Chain as Chain
import qualified Database.Persist.Sqlite as Persist
import qualified Database.Schema as Sql

import Data.DBVar
import Data.Delta

{-------------------------------------------------------------------------------
    (Mock) address type
-------------------------------------------------------------------------------}
type Address = Text
type Node = Word32

data AddressInPool = AddressInPool
    { address :: Address
    , index   :: Word32
    } deriving (Eq, Ord, Show)

-- | Construnct an 'Embedding' of delta encodings from an isomorphism.
embedIso :: Iso' a b -> Embedding [DeltaDB Int a] [DeltaDB Int b]
embedIso i = withIso i $ \ab ba -> mkEmbedding Embedding'
    { load = Just . fmap ba
    , write = fmap ab
    , update = \_ _ -> fmap (fmap ab)
    }

type StoreAddress = Store SqlPersistM (DeltaChain Node [AddressInPool])

{-------------------------------------------------------------------------------
    Store using Persistent entities
-------------------------------------------------------------------------------}
share
    [ mkPersist (sqlSettings { mpsPrefixFields = False })
    , mkMigrate "migrateAll"
    ]
    [persistLowerCase|
SeqStateAddress
    seqStateAddressFrom             Node               sql=from
    seqStateAddressTo               Node               sql=to
    seqStateAddressWalletId         Word32             sql=wallet_id
    seqStateAddressAddress          Address            sql=address
    seqStateAddressIndex            Word32             sql=address_ix
    deriving Generic
|]

instance Show SeqStateAddress where
    show x =
        show (seqStateAddressTo x)
        <> " <--" <> show (seqStateAddressAddress x)
        <> "-- " <> show (seqStateAddressFrom x)

addressDBIso :: Iso' (Edge Node AddressInPool) SeqStateAddress
addressDBIso = iso ab ba
  where
    ab Edge{from,to,via=AddressInPool{address,index}} =
        SeqStateAddress from to 0 address index
    ba (SeqStateAddress from to _ address index) =
        Edge{from,to,via=AddressInPool{address,index}}

addressChainIntoTable
    :: Embedding
        (DeltaChain Node [AddressInPool])
        [DeltaDB Int SeqStateAddress]
addressChainIntoTable = 
    embedIso addressDBIso `o` (tableIntoDatabase `o` chainIntoTable Pile getPile)

newStoreAddress :: SqlPersistM StoreAddress
newStoreAddress = embedStore addressChainIntoTable =<< newEntityStore

{-------------------------------------------------------------------------------
    Store using SQL row types
-------------------------------------------------------------------------------}
-- | 'MonadSTM' instance for the 'SqlPersistM' monad.
instance MonadSTM (NoLoggingT (ResourceT IO)) where
    type STM (NoLoggingT (ResourceT IO)) = STM IO
    atomically = liftIO . atomically

newStoreAddressSql :: SqlPersistM StoreAddress
newStoreAddressSql = do
    Sql.runSql $ Sql.createTable (Proxy :: Proxy (AddressRow :. Col "id" Primary))
    embedStore embed =<< newSqlStore
  where
    embed = embedIso addressSqlIso
        `o` (tableIntoDatabase `o` chainIntoTable Pile getPile)

addressSqlIso :: Iso' (Edge Node AddressInPool) AddressRow
addressSqlIso = iso ab ba
  where
    ab Edge{from,to,via=AddressInPool{address,index}} =
        Table :. Col from :. Col to :. Col 0 :. Col address :. Col index
    ba (Table :. Col from :. Col to :. Col _ :. Col address :. Col index) =
        Edge{from,to,via=AddressInPool{address,index}}

type AddressRow = Table "addresses"
    :. Col "from" Node :. Col "to" Node
    :. Col "wallet_id" Word32
    :. Col "address" Address :. Col "address_ix" Word32

{-------------------------------------------------------------------------------
    Database connection
-------------------------------------------------------------------------------}
main :: IO ()
main = Persist.runSqlite ":memory:" $ do
    Persist.runMigration migrateAll

    store <- newStoreAddressSql
    db    <- initDBVar store
        $ Chain.fromEdge Edge{from=0,to=1,via=[AddressInPool "a" 31]}

    updateDBVar db $ Chain.AppendTip 2 [AddressInPool "b" 32]
    updateDBVar db $ Chain.AppendTip 3 [AddressInPool "c" 33]
    updateDBVar db $ Chain.CollapseNode 2
    updateDBVar db $ Chain.CollapseNode 1

    sayShow =<< readDBVar db
    sayShow =<< loadS store
