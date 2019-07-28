{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Dummy implementation of the database-layer, using MVar. This may be good for
-- state-machine testing in order to compare it with an implementation on a real
-- data store.

module Cardano.Wallet.DB.MVar
    ( newDBLayer
    ) where

import Prelude

import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Key, XPrv )
import Cardano.Wallet.Primitive.Model
    ( Wallet )
import Cardano.Wallet.Primitive.Types
    ( Hash
    , SortOrder (..)
    , Tx
    , TxMeta (slotId)
    , WalletId
    , WalletMetadata
    , isWithinRange
    )
import Control.Concurrent.MVar
    ( MVar, modifyMVar, newMVar, readMVar, withMVar )
import Control.DeepSeq
    ( deepseq )
import Control.Monad
    ( (>=>) )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT )
import Data.List
    ( sortBy )
import Data.Map.Strict
    ( Map )
import Data.Ord
    ( Down (..), comparing )

import qualified Data.Map.Strict as Map

data Database s t = Database
    { wallet :: !(Wallet s t)
    , metadata :: !WalletMetadata
    , txHistory :: !(Map (Hash "Tx") (Tx t, TxMeta))
    , xprv :: !(Maybe (Key 'RootK XPrv, Hash "encryption"))
    }

-- | Instantiate a new in-memory "database" layer that simply stores data in
-- a local MVar. Data vanishes if the software is shut down.
newDBLayer :: forall s t. IO (DBLayer IO s t)
newDBLayer = do
    lock <- newMVar ()
    db <- newMVar (mempty :: Map (PrimaryKey WalletId) (Database s t))
    return $ DBLayer

        {-----------------------------------------------------------------------
                                      Wallets
        -----------------------------------------------------------------------}

        { createWallet = \key@(PrimaryKey wid) cp meta -> ExceptT $ do
            let alter = \case
                    Nothing ->
                        Right $ Just $ Database cp meta mempty Nothing
                    Just _ ->
                        Left (ErrWalletAlreadyExists wid)
            cp `deepseq` meta `deepseq` alterMVar db alter key

        , removeWallet = \key@(PrimaryKey wid) -> ExceptT $ do
            let alter = \case
                    Nothing ->
                        Left (ErrNoSuchWallet wid)
                    Just _ ->
                        Right Nothing
            alterMVar db alter key

        , listWallets =
            Map.keys <$> readMVar db

        {-----------------------------------------------------------------------
                                    Checkpoints
        -----------------------------------------------------------------------}

        , putCheckpoint = \key@(PrimaryKey wid) cp -> ExceptT $ do
            let alter = \case
                    Nothing ->
                        Left (ErrNoSuchWallet wid)
                    Just (Database _ meta history k) ->
                        Right $ Just $ Database cp meta history k
            cp `deepseq` alterMVar db alter key

        , readCheckpoint = \key ->
            fmap wallet . Map.lookup key <$> readMVar db

        {-----------------------------------------------------------------------
                                   Wallet Metadata
        -----------------------------------------------------------------------}

        , putWalletMeta = \key@(PrimaryKey wid) meta -> ExceptT $ do
            let alter = \case
                    Nothing ->
                        Left (ErrNoSuchWallet wid)
                    Just (Database cp _ history k) ->
                        Right $ Just $ Database cp meta history k
            meta `deepseq` alterMVar db alter key

        , readWalletMeta = \key -> do
            fmap metadata . Map.lookup key <$> readMVar db

        {-----------------------------------------------------------------------
                                     Tx History
        -----------------------------------------------------------------------}

        , putTxHistory = \key@(PrimaryKey wid) txs' -> ExceptT $ do
            let alter = \case
                    Nothing ->
                        Left (ErrNoSuchWallet wid)
                    Just (Database cp meta txs k) ->
                        Right $ Just $ Database cp meta (txs' <> txs) k
            txs' `deepseq` alterMVar db alter key

        , readTxHistory = \key order range -> let
                order' = case order of
                    Ascending -> comparing slot
                    Descending -> comparing $ Down . slot
                result =
                    filter (isWithinRange range . slot)
                    . sortBy order' . Map.toList . txHistory
                slot = slotId . snd . snd
            in maybe mempty result . Map.lookup key <$> readMVar db

        {-----------------------------------------------------------------------
                                       Keystore
        -----------------------------------------------------------------------}

        , putPrivateKey = \key@(PrimaryKey wid) k -> ExceptT $ do
            let alter = \case
                    Nothing ->
                        Left (ErrNoSuchWallet wid)
                    Just (Database cp meta txs _) ->
                        Right $ Just $ Database cp meta txs (Just k)
            k `deepseq` alterMVar db alter key

        , readPrivateKey = \key ->
            (Map.lookup key >=> xprv) <$> readMVar db

        {-----------------------------------------------------------------------
                                       Lock
        -----------------------------------------------------------------------}

        , withLock = \action ->
            ExceptT $ withMVar lock $ \() -> runExceptT action
        }


-- | Modify the content of an MVar holding a map with a given alteration
-- function. The map is only modified if the 'Right' branch of the alteration
-- function yields something. See also:
--
-- [Data.Map.Strict#alterF](https://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Strict.html#v:alterF)
alterMVar
    :: Ord k
    => MVar (Map k v) -- MVar holding our mock database
    -> (Maybe v -> Either err (Maybe v)) -- An alteration function
    -> k -- Key to alter
    -> IO (Either err ())
alterMVar db alter key =
    modifyMVar db (\m -> bubble m $ Map.alterF alter key m)
  where
    -- | Re-wrap an error into an MVar result so that it will bubble up
    bubble :: Monad m => a -> Either err a -> m (a, Either err ())
    bubble a = \case
        Left err -> return (a, Left err)
        Right a' -> return (a', Right ())
