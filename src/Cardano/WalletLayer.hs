{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Provides the wallet layer functions that are used by API layer and uses both
-- "Cardano.DBLayer" and "Cardano.NetworkLayer" to realize its role as being
-- intermediary between the three.


module Cardano.WalletLayer where

import Prelude

import Cardano.DBLayer
    ( DBLayer (..), PrimaryKey (..) )
import Cardano.NetworkLayer
    ( NetworkLayer (..), listen )
import Cardano.Wallet
    ( Wallet
    , WalletId (..)
    , WalletName (..)
    , applyBlock
    , availableBalance
    , currentTip
    , initWallet
    )
import Cardano.Wallet.AddressDerivation
    ( ChangeChain (..)
    , Passphrase
    , deriveAccountPrivateKey
    , generateKeyFromSeed
    , publicKey
    )
import Cardano.Wallet.AddressDiscovery
    ( AddressPoolGap, SeqState (..), mkAddressPool )
import Cardano.Wallet.Mnemonic
    ( Mnemonic, entropyToBytes, mnemonicToEntropy )
import Cardano.Wallet.Primitive
    ( Block (..) )
import Control.DeepSeq
    ( deepseq )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT, throwE )
import Data.List
    ( foldl' )
import Data.List.NonEmpty
    ( NonEmpty ((:|)) )
import Fmt
    ( build, (+|), (|+) )
import GHC.Generics
    ( Generic )
import Say
    ( say )

import qualified Data.Set as Set

-- | Types
data WalletLayer m s = WalletLayer
    { createWallet :: NewWallet -> ExceptT CreateWalletError m WalletId
    , getWallet :: WalletId -> ExceptT GetWalletError m (Wallet s)
    , watchWallet :: WalletId -> m ()
    }

data NewWallet = NewWallet
    { mnemonic
        :: !(Mnemonic 15)
    , mnemonic2ndFactor
        :: !(Passphrase "generation")
    , name
        :: !WalletName
    , passphrase
        :: !(Passphrase "encryption")
    , gap
        :: !AddressPoolGap
    } deriving (Show, Generic)

-- | Errors occuring when fetching a wallet
newtype GetWalletError
    = ErrGetWalletNotFound WalletId
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
    -> WalletLayer IO SeqState
mkWalletLayer db network = WalletLayer
    { createWallet = \w -> do
        let seed =
                entropyToBytes $ mnemonicToEntropy (mnemonic w)
        let rootXPrv =
                generateKeyFromSeed (seed, mnemonic2ndFactor w) (passphrase w)
        let accXPrv =
                deriveAccountPrivateKey mempty rootXPrv minBound
        let extPool =
                mkAddressPool (publicKey accXPrv) (gap w) ExternalChain []
        let intPool =
                mkAddressPool (publicKey accXPrv) minBound InternalChain []
        let wallet =
                initWallet $ SeqState (extPool, intPool)
        let wid = WalletId $ getWalletName $ name w
        lift (readCheckpoints db (PrimaryKey wid)) >>= \case
            Nothing -> do
                lift $ putCheckpoints db (PrimaryKey wid) (wallet :| [])
                return wid
            Just _ ->
                throwE $ ErrCreateWalletIdAlreadyExists wid
    , getWallet = \wid -> lift (readCheckpoints db (PrimaryKey wid)) >>= \case
        Nothing ->
            throwE $ ErrGetWalletNotFound wid
        Just (w :| _) ->
            return w

    , watchWallet = \wid ->
        listen network $ \blocks -> applyBlocks wid blocks *> printInfo wid
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

    printInfo :: WalletId -> IO ()
    printInfo wid = readCheckpoints db (PrimaryKey wid) >>= \case
        Nothing ->
            say "No available checkpoints"
        Just (cp :| _) -> do
            say $ "Current tip: " +| build (currentTip cp)
            say $ "Available balance: " +| (availableBalance cp) |+ " Lovelaces"
