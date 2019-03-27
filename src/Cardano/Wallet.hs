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


module Cardano.Wallet where

import Prelude

import Cardano.Wallet.DB
    ( DBLayer (..), PrimaryKey (..) )
import Cardano.Wallet.Network
    ( NetworkLayer (..), listen )
import Cardano.Wallet.Primitive.AddressDerivation
    ( ChangeChain (..)
    , Passphrase
    , deriveAccountPrivateKey
    , generateKeyFromSeed
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressPoolGap, SeqState (..), mkAddressPool )
import Cardano.Wallet.Primitive.Model
    ( Wallet, applyBlock, initWallet )
import Cardano.Wallet.Primitive.Types
    ( Block (..), WalletId (..), WalletMetadata (..), WalletName (..) )
import Control.DeepSeq
    ( deepseq )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, throwE )
import Data.List
    ( foldl' )
import Data.List.NonEmpty
    ( NonEmpty ((:|)) )
import GHC.Generics
    ( Generic )

import qualified Data.Set as Set

-- | Types
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


-- | Create a new instance of the wallet layer.
mkWalletLayer
    :: (Show e0)
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
        -- FIXME Compute the wallet id deterministically from the seed
        let wid = WalletId (read "00000000-0000-0000-0000-000000000000")
        liftIO (readCheckpoints db (PrimaryKey wid)) >>= \case
            Nothing -> do
                liftIO $ putCheckpoints db (PrimaryKey wid) (wallet :| [])
                return wid
            Just _ ->
                throwE $ ErrCreateWalletIdAlreadyExists wid
    , readWallet = \wid -> liftIO (readCheckpoints db (PrimaryKey wid)) >>= \case
        Nothing ->
            throwE $ ErrReadWalletNotFound wid
        Just (w :| _) ->
            return (w, error "FIXME: store and retrieve wallet metadata")

    , watchWallet = liftIO . listen network . applyBlocks
    }
  where
    applyBlocks :: WalletId -> [Block] -> IO ()
    applyBlocks wid blocks = do
        cps' <- readCheckpoints db (PrimaryKey wid) >>= \case
            Nothing ->
                fail $ "couldn't find worker wallet: " <> show wid
            Just cps -> do
                let nonEmpty = not . Set.null . transactions
                let cps' = foldl' (flip applyBlock) cps (filter nonEmpty blocks)
                return cps'
        cps' `deepseq` putCheckpoints db (PrimaryKey wid) cps'
