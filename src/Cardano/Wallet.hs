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
    , ErrNoSuchWallet(..)
    , ErrWalletAlreadyExists(..)

    -- * Construction
    , mkWalletLayer

    -- * Helpers
    , unsafeRunExceptT
    ) where

import Prelude

import Cardano.Wallet.DB
    ( DBLayer
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    )
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
    ( Block (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)
    )
import Control.Exception
    ( Exception )
import Control.Monad
    ( (>=>) )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Control.Monad.Trans.Maybe
    ( MaybeT (..), maybeToExceptT )
import Data.Functor
    ( ($>) )
import Data.Time.Clock
    ( getCurrentTime )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.DB as DB

{-------------------------------------------------------------------------------
                                 Types
-------------------------------------------------------------------------------}

data WalletLayer s = WalletLayer
    { createWallet
        :: NewWallet
        -> ExceptT (ErrWalletAlreadyExists "createWallet") IO WalletId
    , readWallet
        :: WalletId
        -> ExceptT (ErrNoSuchWallet "readWallet") IO (Wallet s, WalletMetadata)
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
        let wid =
                WalletId (digest $ publicKey rootXPrv)
        let checkpoint = initWallet $ SeqState
                { externalPool = extPool
                , internalPool = intPool
                }
        now <- liftIO getCurrentTime
        let metadata = WalletMetadata
                { name = Cardano.Wallet.name w
                , passphraseInfo = WalletPassphraseInfo now
                , status = Restoring minBound
                , delegation = NotDelegating
                }
        DB.createWallet db (PrimaryKey wid) checkpoint metadata $> wid

    , readWallet = \wid -> maybeToExceptT (ErrNoSuchWallet wid) $ do
        cp <- MaybeT $ DB.readCheckpoint db (PrimaryKey wid)
        meta <- MaybeT $ DB.readWalletMeta db (PrimaryKey wid)
        return (cp, meta)

    , watchWallet =
        liftIO . listen network . onNextblocks
    }
  where
    onNextblocks :: WalletId -> [Block] -> IO ()
    onNextblocks wid blocks = do
        (txs, cp') <- DB.readCheckpoint db (PrimaryKey wid) >>= \case
            Nothing ->
                fail $ "couldn't find worker wallet: " <> show wid
            Just cp -> do
                let nonEmpty = not . null . transactions
                return $ applyBlocks (filter nonEmpty blocks) cp
        -- FIXME
        -- Note that, the two calls below are _safe_ under the assumption that
        -- the wallet existed _right before_. In theory, in a multi-threaded
        -- context, it may happen that another actor deletes the wallet between
        -- the calls. Here
        -- In practice, it isn't really _bad_ if the wallet is gone, we could
        -- simply log an error or warning and move on. This would have to be
        -- done as soon as we introduce logging.
        -- Note also that there's no transaction surrounding both calls because
        -- there's only one thread per wallet that will apply blocks. And
        -- therefore, only one thread making changes on checkpoints and/or tx
        -- history
        unsafeRunExceptT $ DB.putCheckpoint db (PrimaryKey wid) cp'
        unsafeRunExceptT $ DB.putTxHistory db (PrimaryKey wid) txs

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
