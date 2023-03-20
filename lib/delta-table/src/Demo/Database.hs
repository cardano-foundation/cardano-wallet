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
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Demo.Database where

import Prelude

import Conduit
    ( ResourceT )
import Control.Applicative
    ( Alternative )
import Control.Monad
    ( MonadPlus )
import Control.Monad.Class.MonadSTM.Internal
    ( MonadSTM (..) )
import Control.Monad.Class.MonadThrow
    ( ExitCase (..)
    , MonadCatch (..)
    , MonadEvaluate (..)
    , MonadMask (..)
    , MonadThrow (..)
    )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Logger
    ( NoLoggingT )
import Data.Chain
    ( DeltaChain (..), Edge (..), chainIntoTable )
import Data.Function
    ( on )
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

import qualified Control.Monad.Catch as ResourceT
import qualified Control.Monad.STM as STM
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
    { load = Right . fmap ba
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
--
-- NB. This is missing most of the STM methods except for a handful of TVar
-- ones!
instance MonadSTM (NoLoggingT (ResourceT IO)) where
    type STM (NoLoggingT (ResourceT IO)) = WrapSTM
    type TVar (NoLoggingT (ResourceT IO)) = TVar IO
    newTVar        = WrapSTM .  newTVar
    readTVar       = WrapSTM .  readTVar
    writeTVar      = WrapSTM .: writeTVar
    retry          = WrapSTM    retry
    orElse         = WrapSTM .: on orElse unWrapSTM

    modifyTVar     = WrapSTM .: modifyTVar
    modifyTVar'    = WrapSTM .: modifyTVar'
    stateTVar      = WrapSTM .: stateTVar
    swapTVar       = WrapSTM .: swapTVar
    check          = WrapSTM  . check

    type TMVar (NoLoggingT (ResourceT IO)) = TMVar IO
    newTMVar       = WrapSTM .  newTMVar
    newEmptyTMVar  = WrapSTM    newEmptyTMVar
    takeTMVar      = WrapSTM .  takeTMVar
    tryTakeTMVar   = WrapSTM .  tryTakeTMVar
    putTMVar       = WrapSTM .: putTMVar
    tryPutTMVar    = WrapSTM .: tryPutTMVar
    readTMVar      = WrapSTM .  readTMVar
    tryReadTMVar   = WrapSTM .  tryReadTMVar
    swapTMVar      = WrapSTM .: swapTMVar
    isEmptyTMVar   = WrapSTM .  isEmptyTMVar

    type TQueue (NoLoggingT (ResourceT IO)) = TQueue IO
    newTQueue      = WrapSTM newTQueue
    readTQueue     = WrapSTM .  readTQueue
    tryReadTQueue  = WrapSTM .  tryReadTQueue
    peekTQueue     = WrapSTM .  peekTQueue
    tryPeekTQueue  = WrapSTM .  tryPeekTQueue
    flushTQueue    = WrapSTM .  flushTQueue
    writeTQueue v  = WrapSTM .  writeTQueue v
    isEmptyTQueue  = WrapSTM .  isEmptyTQueue
    unGetTQueue    = WrapSTM .: unGetTQueue

    type TBQueue (NoLoggingT (ResourceT IO)) = TBQueue IO
    newTBQueue     = WrapSTM .  newTBQueue
    readTBQueue    = WrapSTM .  readTBQueue
    tryReadTBQueue = WrapSTM .  tryReadTBQueue
    peekTBQueue    = WrapSTM .  peekTBQueue
    tryPeekTBQueue = WrapSTM .  tryPeekTBQueue
    flushTBQueue   = WrapSTM .  flushTBQueue
    writeTBQueue   = WrapSTM .: writeTBQueue
    lengthTBQueue  = WrapSTM .  lengthTBQueue
    isEmptyTBQueue = WrapSTM .  isEmptyTBQueue
    isFullTBQueue  = WrapSTM .  isFullTBQueue
    unGetTBQueue   = WrapSTM .: unGetTBQueue

    type TArray (NoLoggingT (ResourceT IO)) = TArray IO

    type TSem (NoLoggingT (ResourceT IO)) = TSem IO
    newTSem        = WrapSTM .  newTSem
    waitTSem       = WrapSTM .  waitTSem
    signalTSem     = WrapSTM .  signalTSem
    signalTSemN    = WrapSTM .: signalTSemN

    type TChan (NoLoggingT (ResourceT IO)) = TChan IO
    newTChan          = WrapSTM    newTChan
    newBroadcastTChan = WrapSTM    newBroadcastTChan
    dupTChan          = WrapSTM .  dupTChan
    cloneTChan        = WrapSTM .  cloneTChan
    readTChan         = WrapSTM .  readTChan
    tryReadTChan      = WrapSTM .  tryReadTChan
    peekTChan         = WrapSTM .  peekTChan
    tryPeekTChan      = WrapSTM .  tryPeekTChan
    writeTChan        = WrapSTM .: writeTChan
    unGetTChan        = WrapSTM .: unGetTChan
    isEmptyTChan      = WrapSTM .  isEmptyTChan


(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(f .: g) x y = f (g x y)

-- | Helper type for the above instance.
newtype WrapSTM a = WrapSTM { unWrapSTM :: STM.STM a }
    deriving (Applicative, Functor, Monad)

deriving instance MonadPlus WrapSTM
deriving instance Alternative WrapSTM

-- "Exceptional monads" instances for the 'SqlPersistM' monad.
instance MonadEvaluate (NoLoggingT (ResourceT IO)) where
    evaluate = liftIO . evaluate

instance MonadThrow (NoLoggingT (ResourceT IO)) where
    throwIO = ResourceT.throwM

instance MonadCatch (NoLoggingT (ResourceT IO)) where
    catch = ResourceT.catch
    generalBracket before after =
        ResourceT.generalBracket before (\a -> after a . contra)
      where
        contra (ResourceT.ExitCaseSuccess a) = ExitCaseSuccess a
        contra (ResourceT.ExitCaseException e) = ExitCaseException e
        contra (ResourceT.ExitCaseAbort) = ExitCaseAbort

instance MonadMask (NoLoggingT (ResourceT IO)) where
    mask = ResourceT.mask
    uninterruptibleMask = ResourceT.uninterruptibleMask


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
