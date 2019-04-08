{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Provides the wallet layer functions that are used by API layer and uses both
-- "Cardano.Wallet.DB" and "Cardano.Wallet.Network" to realize its role as being
-- intermediary between the three.


module Cardano.Wallet
    (
    -- * Types
      WalletLayer (..)
    , NewWallet(..)
    , ReadWalletError(..)
    , CreateWalletError(..)

    -- * Construction
    , mkWalletLayer

    -- * Helpers
    , unsafeRunExceptT
    ) where

import Prelude

import Cardano.Wallet.DB
    ( DBLayer (..), PrimaryKey (..) )
import Cardano.Wallet.Network
    ( NetworkLayer (..), listen )
import Cardano.Wallet.Primitive.AddressDerivation
    ( ChangeChain (..)
    , Passphrase
    , deriveAccountPrivateKey
    , digest
    , generateKeyFromSeed
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressPoolGap, SeqState (..), mkAddressPool )
import Cardano.Wallet.Primitive.Model
    ( Wallet, applyBlocks, initWallet )
import Cardano.Wallet.Primitive.Types
    ( Block (..), WalletId (..), WalletMetadata (..), WalletName (..) )
import Control.Exception
    ( Exception )
import Control.Monad
    ( (>=>) )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT, throwE )
import GHC.Generics
    ( Generic )


{-------------------------------------------------------------------------------
                                 Types
-------------------------------------------------------------------------------}

data WalletLayer s = WalletLayer
    { createWallet
        :: NewWallet
        -> ExceptT CreateWalletError IO WalletId
    , readWallet
        :: WalletId
        -> ExceptT ReadWalletError IO (Wallet s, WalletMetadata)
    , watchWallet
        :: WalletId
        -> IO ()
    }

data NewWallet = NewWallet
    { seed
        :: !(Passphrase "seed")
    , secondFactor
        :: !(Passphrase "generation")
    , name
        :: !WalletName
    , passphrase
        :: !(Passphrase "encryption")
    , gap
        :: !AddressPoolGap
    } deriving (Show, Generic)

-- | Errors occuring when fetching a wallet
newtype ReadWalletError
    = ErrReadWalletNotFound WalletId
    deriving (Eq, Show)

-- | Errors occuring when creating a wallet
newtype CreateWalletError
    = ErrCreateWalletIdAlreadyExists WalletId
    deriving (Eq, Show)

{-------------------------------------------------------------------------------
                                 Construction
-------------------------------------------------------------------------------}

-- | Create a new instance of the wallet layer.
mkWalletLayer
    :: (Exception e0)
    => DBLayer IO SeqState
    -> NetworkLayer IO e0 e1
    -> WalletLayer SeqState
mkWalletLayer db network = WalletLayer
    { createWallet = \w -> do
        let rootXPrv =
                generateKeyFromSeed (seed w, secondFactor w) (passphrase w)
        let accXPrv =
                deriveAccountPrivateKey mempty rootXPrv minBound
        let extPool =
                mkAddressPool (publicKey accXPrv) (gap w) ExternalChain []
        let intPool =
                mkAddressPool (publicKey accXPrv) minBound InternalChain []
        let wallet = initWallet $ SeqState
                { externalPool = extPool
                , internalPool = intPool
                }
        let wid = WalletId (digest $ publicKey rootXPrv)
        liftIO (readCheckpoint db (PrimaryKey wid)) >>= \case
            Nothing -> do
                liftIO $ putCheckpoint db (PrimaryKey wid) wallet
                return wid
            Just _ ->
                throwE $ ErrCreateWalletIdAlreadyExists wid
    , readWallet = \wid -> liftIO (readCheckpoint db (PrimaryKey wid)) >>= \case
        Nothing ->
            throwE $ ErrReadWalletNotFound wid
        Just w ->
            return (w, error "FIXME: store and retrieve wallet metadata")

    , watchWallet = liftIO . listen network . onNextblocks
    }
  where
    onNextblocks :: WalletId -> [Block] -> IO ()
    onNextblocks wid blocks = do
        (txs, cp') <- readCheckpoint db (PrimaryKey wid) >>= \case
            Nothing ->
                fail $ "couldn't find worker wallet: " <> show wid
            Just cp -> do
                let nonEmpty = not . null . transactions
                return $ applyBlocks (filter nonEmpty blocks) cp
        putCheckpoint db (PrimaryKey wid) cp'
        unsafeRunExceptT $ putTxHistory db (PrimaryKey wid) txs -- Safe after ^

{-------------------------------------------------------------------------------
                                 Helpers
-------------------------------------------------------------------------------}

-- | Run an ExcepT and throws the error if any. This makes sense only if called
-- after checking for an invariant or, after ensuring that preconditions for
-- meeting the underlying error have been discarded.
unsafeRunExceptT :: (MonadFail m, Show e) => ExceptT e m a -> m a
unsafeRunExceptT = runExceptT >=> \case
    Left e ->
        fail $ "unexpected error: " <> show e
    Right a ->
        return a
