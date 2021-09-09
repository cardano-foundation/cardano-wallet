{-# LANGUAGE LambdaCase #-}
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

import Prelude hiding (all)

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
    ( MonadIO, liftIO )
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
import Data.Table
    ( DeltaDB (..)
    , Table (..)
    , tableIntoDatabase
    , Pile (..)
    )
import GHC.Generics
    ( Generic )
import Say
    ( say, sayShow )

-- FIXME: Replace with IOSim stuff later.
import Data.IORef
    ( IORef, newIORef, readIORef, writeIORef )

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
    deriving Generic
|]

instance Show SeqStateAddress where
    show x =
        show (seqStateAddressTo x)
        <> " <--" <> show (seqStateAddressAddress x)
        <> "-- " <> show (seqStateAddressFrom x)

addressDBIso :: Iso' (Edge Node AddressInPool) SeqStateAddress
addressDBIso = iso
    (\Edge{from,to,via=AddressInPool{address,index}}
        -> SeqStateAddress from to 0 address index)
    (\(SeqStateAddress from to _ address index)
        -> Edge{from,to,via=AddressInPool{address,index}})

addressChainIntoTable
    :: Embedding
        (DeltaChain Node [AddressInPool])
        [DeltaDB Int SeqStateAddress]
addressChainIntoTable = 
    embedIso addressDBIso `o` (tableIntoDatabase `o` chainIntoTable Pile getPile)

embedIso :: Iso' a b -> Embedding [DeltaDB Int a] [DeltaDB Int b]
embedIso i = withIso i $ \ab ba -> mkEmbedding Embedding'
    { load = Just . fmap ba
    , write = fmap ab
    , update = \_ _ -> fmap (fmap ab)
    }

type AddressStore =
    Store DBIO (DeltaChain Node [AddressInPool]) (Chain Node [AddressInPool])

newAddressStore :: DBIO AddressStore
newAddressStore = embedStore addressChainIntoTable =<< newDBStore

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
    ( PersistRecordBackend record SqlBackend
    , ToBackendKey SqlBackend record, Show record
    , MonadIO m )
    => m (Store DBIO [DeltaDB Int record] (Table record))
newDBStore = do
    ref <- liftIO $ newIORef Nothing
    let rememberSupply table = liftIO $ writeIORef ref $ Just $ uids table
    pure $ Store
        { loadS   = do
            debug $ do
                say "\n** loadS"
                liftIO . print =<< selectList all []
            -- read database table, preserve keys
            let toPair (Entity key val) = (fromIntegral $ fromSqlKey key,val)
            table <- Table.fromRows . map toPair <$> selectList all []
            -- but use our own unique ID supply
            liftIO (readIORef ref) >>= \case
                Just supply  -> pure $ Just table{uids = supply}
                Nothing      -> do
                    rememberSupply table
                    pure $ Just table
        , writeS  = \table -> void $ do
            deleteWhere all -- delete any old data in the table first
            _ <- insertMany $ getPile $ Table.toPile table
            rememberSupply table
        , updateS = \table ds -> do
            debug $ do
                say "\n** updateS table deltas"
                sayShow $ table
                sayShow $ ds
            mapM_ (update1 table) ds
            rememberSupply (apply ds table) -- need to use updated supply
        }
  where
    debug m = if False then m else pure ()

    all :: [Filter record]
    all = []

    toKey :: Int -> Key record
    toKey = toSqlKey . fromIntegral

    update1 _ (InsertManyDB zs) = void $
        repsertMany  [ (toKey key, value) | (key, value) <- zs ]
    update1 _ (DeleteManyDB ks) = void $ forM ks $ delete . toKey
    update1 _ (UpdateManyDB zs) = void $ forM zs $ \(key, value) ->
        replace (toKey key) value

{- Note [Unique ID supply in newDBStore]

We expect that updating the store and loading the value
is the same as first loading the value and then apply the delta,
i.e. we expect that the two actions

    loadS >>= \a -> updateS a da >>= loadS
    loadS >>= \a -> pure $ apply da a

are operationally equivalent.
However, this is only the case if we keep track of the supply
of unique IDs for the table! Otherwise, loading the table
from the database again can mess up the supply.
-}
-- FIXME: For clarity, we may want to implement this in terms
-- of a product of stores ("semidirect product").

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
    (liftIO . print) =<< loadS store
