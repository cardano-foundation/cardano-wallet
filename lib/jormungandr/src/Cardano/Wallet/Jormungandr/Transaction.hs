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
    ( toXPub, xpubPublicKey )
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
    ( ChimericAccount (..)
    , Hash (..)
    , SealedTx (..)
    , Tx (..)
    , TxMetadata
    , TxOut (..)
    )
import Cardano.Wallet.Transaction
    ( DelegationAction (..)
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

import qualified Cardano.Wallet.Primitive.CoinSelection as CS
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
    { mkStdTx = \_rewardAcnt keyFrom _ _ cs ->
        mkFragment
            ( MkFragmentSimpleTransaction (txWitnessTagFor @k)
            ) keyFrom (CS.inputs cs) (CS.outputs cs)

    , mkDelegationJoinTx = \pool accXPrv keyFrom _ cs ->
        let acc = ChimericAccount . xpubPublicKey . toXPub . fst $ accXPrv
        in mkFragment
            ( MkFragmentStakeDelegation
                (txWitnessTagFor @k)
                (DlgFull pool)
                acc
                accXPrv
            ) keyFrom (CS.inputs cs) (CS.outputs cs)

    , mkDelegationQuitTx = \accXPrv keyFrom _ cs ->
        let acc = ChimericAccount . xpubPublicKey . toXPub . fst $ accXPrv
        in mkFragment
            ( MkFragmentStakeDelegation
                (txWitnessTagFor @k)
                DlgNone
                acc
                accXPrv
            ) keyFrom (CS.inputs cs) (CS.outputs cs)

    , initDelegationSelection = const mempty

    , decodeSignedTx = \payload -> do
        let errInvalidPayload =
                ErrDecodeSignedTxWrongPayload "wrongly constructed binary blob"
        case runGetOrFail getFragment (BL.fromStrict payload) of
            Left _ -> Left errInvalidPayload
            Right (_,_,msg) -> case msg of
                Transaction tx -> return (tx, SealedTx payload)
                _ -> Left errInvalidPayload

    , minimumFee = _minimumFee

    , estimateMaxNumberOfInputs = \_ _ _ -> fromIntegral maxNumberOfInputs

    , validateSelection = \cs -> do
        let tooManyInputs  = length (CS.inputs cs)  > maxNumberOfInputs
        let tooManyOutputs = length (CS.outputs cs) > maxNumberOfOutputs
        when (tooManyInputs || tooManyOutputs)
            $ Left ErrExceededInpsOrOuts

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
                , withdrawals = mempty
                , metadata = Nothing
                }
            , finalizeFragment fragment
            )

    -- NOTE
    -- Jörmungandr fee calculation is a linear function where the coefficient
    -- is multiplied by the total number of inputs and outputs.
    _minimumFee
        :: FeePolicy
        -> Maybe DelegationAction
        -> Maybe TxMetadata
        -> CoinSelection
        -> Fee
    _minimumFee policy action _ cs =
        Fee $ ceiling (a + b*fromIntegral ios + c*certs)
      where
        LinearFee (Quantity a) (Quantity b) (Quantity c) = policy
        certs = if isJust action then 1 else 0
        ios = length (CS.inputs cs) + length (CS.outputs cs) + length (CS.change cs)

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
    deriving (Eq, Show)

instance Buildable ErrValidateSelection where
    build = \case
        ErrExceededInpsOrOuts -> build $ T.unwords
            [ "I can't validate coin selection because either the number of inputs"
            , "is more than", maxI, "or the number of outputs exceeds", maxO <> "."
            ]
      where
        maxI = toText maxNumberOfInputs
        maxO = toText maxNumberOfOutputs

type instance W.ErrValidateSelection Jormungandr = ErrValidateSelection
