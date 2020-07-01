{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Jormungandr.Transaction
    ( newTransactionLayer
    , ErrValidateSelection (..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( xpubPublicKey )
import Cardano.Wallet.Jormungandr.Binary
    ( Fragment (..)
    , MkFragment (..)
    , StakeDelegationType (..)
    , TxWitnessTag (..)
    , finalizeFragment
    , fragmentId
    , getFragment
    , maxNumberOfInputs
    , maxNumberOfOutputs
    , putFragment
    , runGetOrFail
    )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth, WalletKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Jormungandr
    ( JormungandrKey )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Fee
    ( Fee (..), FeePolicy (..) )
import Cardano.Wallet.Primitive.Types
    ( ChimericAccount (..), Hash (..), SealedTx (..), Tx (..), TxOut (..) )
import Cardano.Wallet.Transaction
    ( Certificate (..)
    , ErrDecodeSignedTx (..)
    , ErrMkTx (..)
    , TransactionLayer (..)
    )
import Control.Arrow
    ( first, second )
import Control.Monad
    ( forM, when )
import Data.Either.Combinators
    ( maybeToRight )
import Data.Maybe
    ( isJust )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( toText )
import Fmt
    ( Buildable (..) )

import qualified Cardano.Wallet.Transaction as W
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

-- | Construct a 'TransactionLayer' compatible with Jormungandr and 'Jörmungandr'
newTransactionLayer
    :: forall k t.
        ( t ~ Jormungandr
        , TxWitnessTagFor k
        , WalletKey k
        )
    => Hash "Genesis"
    -> TransactionLayer t k
newTransactionLayer block0H = TransactionLayer
    { mkStdTx = \keyFrom _ inps outs ->
        mkFragment
            ( MkFragmentSimpleTransaction (txWitnessTagFor @k)
            ) keyFrom inps outs

    , mkDelegationJoinTx = \_ _ pool accXPrv keyFrom _ inps outs chgs ->
        let acc = ChimericAccount . xpubPublicKey . getRawKey . publicKey . fst $ accXPrv
        in mkFragment
            ( MkFragmentStakeDelegation
                (txWitnessTagFor @k)
                (DlgFull pool)
                acc
                (first getRawKey accXPrv)
            ) keyFrom inps (outs ++ chgs)

    , mkDelegationQuitTx = \_ accXPrv keyFrom _ inps outs chgs ->
        let acc = ChimericAccount . xpubPublicKey . getRawKey . publicKey . fst $ accXPrv
        in mkFragment
            ( MkFragmentStakeDelegation
                (txWitnessTagFor @k)
                DlgNone
                acc
                (first getRawKey accXPrv)
            ) keyFrom inps (outs ++ chgs)

    , decodeSignedTx = \payload -> do
        let errInvalidPayload =
                ErrDecodeSignedTxWrongPayload "wrongly constructed binary blob"
        case runGetOrFail getFragment (BL.fromStrict payload) of
            Left _ -> Left errInvalidPayload
            Right (_,_,msg) -> case msg of
                Transaction tx -> return (tx, SealedTx payload)
                _ -> Left errInvalidPayload

    , minimumFee = _minimumFee

    , estimateMaxNumberOfInputs = \_ _ -> fromIntegral maxNumberOfInputs

    , validateSelection = \(CoinSelection inps outs _ rsv) -> do
        when (length inps > maxNumberOfInputs || length outs > maxNumberOfOutputs)
            $ Left ErrExceededInpsOrOuts
        when (isJust rsv)
            $ Left ErrReserveNotAllowed

    , allowUnbalancedTx = False
    }
  where
    mkFragment details keyFrom rnps outs = do
        credentials <- forM rnps $ \(_, TxOut addr _) -> first getRawKey <$>
            maybeToRight (ErrKeyNotFoundForAddress addr) (keyFrom addr)
        let inps = fmap (second coin) rnps
        let fragment = putFragment
                block0H
                (zip inps credentials)
                outs
                details
        return
            ( Tx
                { txId = fragmentId fragment
                , resolvedInputs = inps
                , outputs = outs
                }
            , finalizeFragment fragment
            )

    -- NOTE
    -- Jörmungandr fee calculation is a linear function where the coefficient
    -- is multiplied by the total number of inputs and outputs.
    _minimumFee
        :: FeePolicy
        -> [Certificate]
        -> CoinSelection
        -> Fee
    _minimumFee policy certs (CoinSelection inps outs chgs _) =
        Fee $ ceiling (a + b*fromIntegral ios + c*fromIntegral cs)
      where
        LinearFee (Quantity a) (Quantity b) (Quantity c) = policy
        cs  = length $ filter (/= KeyRegistrationCertificate) certs
        ios = length inps + length outs + length chgs

-- | Provide a transaction witness for a given private key. The type of witness
-- is different between types of keys and, with backward-compatible support, we
-- need to support many types for one backend target.
--
-- We have tightly coupled the type of witness to the type of key for because:
--
-- - ByronKey can only be used with legacy / Byron wallets as they require a
--   special address structure (to embed the derivation path).
--
-- - JormungandrKey could theorically be used with the legacy address structure (as
-- Yoroi does) however, our implementation only associate JormungandrKey to new
-- addresses.
class TxWitnessTagFor (k :: Depth -> * -> *) where
    txWitnessTagFor :: TxWitnessTag

instance TxWitnessTagFor JormungandrKey where txWitnessTagFor = TxWitnessUTxO
instance TxWitnessTagFor IcarusKey      where txWitnessTagFor = TxWitnessLegacyUTxO
instance TxWitnessTagFor ByronKey       where txWitnessTagFor = TxWitnessLegacyUTxO

data ErrValidateSelection
    = ErrExceededInpsOrOuts
        -- ^ Transaction with improper number of inputs and outputs is tried
    | ErrReserveNotAllowed
        -- ^ Transaction has a reserve input amount, not allowed for this backend
    deriving (Eq, Show)

instance Buildable ErrValidateSelection where
    build = \case
        ErrExceededInpsOrOuts -> build $ T.unwords
            [ "I can't validate coin selection because either the number of inputs"
            , "is more than", maxI, "or the number of outputs exceeds", maxO <> "."
            ]
        ErrReserveNotAllowed -> build $ T.unwords
            [ "The given coin selection was given a reserve amount and this is"
            , "not allowed for this backend / era."
            ]
      where
        maxI = toText maxNumberOfInputs
        maxO = toText maxNumberOfOutputs

type instance W.ErrValidateSelection Jormungandr = ErrValidateSelection
