{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Demo.Database where

import Prelude

import Data.Chain
    ( Chain
    , DeltaChain (..)
    , Edge (..)
    , chainIntoTable
    )
import Data.DBVar
import Data.Delta

import Database.Persist
    hiding ( update )
import Database.Persist.Sqlite
    hiding ( update )

import Conduit
    ( ResourceT )
import Control.Monad
    ( forM, void )
import Control.Monad.Class.MonadSTM
    ( MonadSTM (..) )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Logger
    ( NoLoggingT )
import Control.Monad.Trans.Reader
    ( ReaderT (..) )
import Data.Generics.Internal.VL
    ( Iso', iso, withIso )
import Data.Word
    ( Word32 )
import Data.Text
    ( Text )
import Database.Persist.TH
    ( mkMigrate, mkPersist, mpsPrefixFields, persistLowerCase, share, sqlSettings )
import Data.Set
    ( Set )
import Data.Table
    ( DeltaDB (..)
    , Table
    , tableIntoDatabase
    )
import GHC.Generics
    ( Generic )

import qualified Data.Chain as Chain
import qualified Data.Set as Set
import qualified Data.Table as Table
import qualified Database.Persist as DB

{-------------------------------------------------------------------------------
    Types for the database
-------------------------------------------------------------------------------}
type Address = Text
type Node = Word32

data AddressInPool = AddressInPool
    { address :: Address
    , index   :: Word32
    } deriving (Eq, Ord, Show)

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
    deriving Show Generic
|]

addressDBIso :: Iso' (Edge Node AddressInPool) SeqStateAddress
addressDBIso = iso
    (\Edge{from,to,via=AddressInPool{address,index}}
        -> SeqStateAddress from to 0 address index)
    (\(SeqStateAddress from to _ address index)
        -> Edge{from,to,via=AddressInPool{address,index}})

addressChainIntoTable
    :: Embedding
        (Chain Node [AddressInPool]) (DeltaChain Node [AddressInPool])
        (Table SeqStateAddress) [DeltaDB Int SeqStateAddress]
addressChainIntoTable = 
    embedIso addressDBIso `compose` (tableIntoDatabase `compose` chainIntoTable)

embedIso
    :: Iso' a b
    -> Embedding
        (Table a) [DeltaDB key a]
        (Table b) [DeltaDB key b]
embedIso iso = withIso iso $ \ab ba -> Embedding
    { load = Just . fmap ba
    , write = fmap ab
    , update = \_ -> fmap (fmap ab)
    }

type AddressStore =
    Store DBIO (DeltaChain Node [AddressInPool]) (Chain Node [AddressInPool])

newAddressStore :: Monad m => m AddressStore
newAddressStore = embedStore addressChainIntoTable <$> newDBStore

{-------------------------------------------------------------------------------
    Database connection
-------------------------------------------------------------------------------}
-- Database monad required by `persistent`.
type DBIO = ReaderT SqlBackend (NoLoggingT (ResourceT IO))
instance MonadSTM (NoLoggingT (ResourceT IO)) where
    type instance STM (NoLoggingT (ResourceT IO)) = STM IO
    atomically = liftIO . atomically

-- FIXME: This function should also do "migrations", i.e.
-- create the database table in the first place.
newDBStore
    :: forall record m.
    ( PersistEntity record, PersistEntityBackend record ~ SqlBackend
    , ToBackendKey SqlBackend record, Show record
    , Monad m )
    => m (Store DBIO [DeltaDB Int record] (Table record))
newDBStore = pure $ Store
    { loadS   = do
        -- liftIO . print =<< selectList all []
        Just . Table.fromList . map entityVal <$> selectList all []
    , writeS  = \table -> void $ do
        deleteWhere all -- first, we delete the table from the database!
        insertMany $ Table.toList table
    , updateS = mapM_ . update1
    }
  where
    all :: [Filter record]
    all = []

    toKey :: Int -> Key record
    toKey = toSqlKey . fromIntegral

    update1 _ (InsertManyDB zs) = void $
        repsertMany  [ (toKey key, value) | (key, value) <- zs ]
    update1 _ (DeleteManyDB ks) = void $ forM ks $ delete . toKey
    update1 _ (UpdateManyDB zs) = void $ forM zs $ \(key, value) ->
        replace (toKey key) value

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll

    store <- newAddressStore
    db    <- initDBVar store
        $ Chain.fromEdge Edge{from=0,to=1,via=[AddressInPool "a" 31]}

    updateDBVar db $ Chain.AppendTip 2 [AddressInPool "b" 32]
    updateDBVar db $ Chain.AppendTip 3 [AddressInPool "c" 33]
    updateDBVar db $ Chain.CollapseNode 2
    updateDBVar db $ Chain.CollapseNode 1

    (liftIO . print) =<< readDBVar db
