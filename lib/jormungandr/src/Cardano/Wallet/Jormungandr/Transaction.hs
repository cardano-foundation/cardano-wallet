{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Jormungandr.Transaction
    ( newTransactionLayer
    , ErrExceededInpsOrOuts (..)
    ) where

import Prelude

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
    ( Depth, WalletKey (..), toChimericAccount )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Types
    ( Hash (..), SealedTx (..), Tx (..), TxOut (..) )
import Cardano.Wallet.Transaction
    ( ErrDecodeSignedTx (..)
    , ErrMkTx (..)
    , ErrValidateSelection
    , TransactionLayer (..)
    )
import Control.Arrow
    ( first, second )
import Control.Monad
    ( forM, when )
import Data.Either.Combinators
    ( maybeToRight )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( toText )
import Fmt
    ( Buildable (..) )

import qualified Data.ByteString.Lazy as BL

-- | Construct a 'TransactionLayer' compatible with Shelley and 'Jörmungandr'
newTransactionLayer
    :: forall k t.
        ( t ~ Jormungandr
        , TxWitnessTagFor k
        , WalletKey k
        )
    => Hash "Genesis"
    -> TransactionLayer t k
newTransactionLayer block0H = TransactionLayer
    { mkStdTx = mkFragment $ MkFragmentSimpleTransaction (txWitnessTagFor @k)

    , mkDelegationJoinTx = \pool accXPrv ->
        let acc = toChimericAccount . publicKey . fst $ accXPrv
        in mkFragment $ MkFragmentStakeDelegation
                    (txWitnessTagFor @k)
                    (DlgFull pool)
                    acc
                    (first getRawKey accXPrv)

    , mkDelegationQuitTx = \accXPrv ->
        let acc = toChimericAccount . publicKey . fst $ accXPrv
        in mkFragment $ MkFragmentStakeDelegation
                    (txWitnessTagFor @k)
                    DlgNone
                    acc
                    (first getRawKey accXPrv)

    , decodeSignedTx = \payload -> do
        let errInvalidPayload =
                ErrDecodeSignedTxWrongPayload "wrongly constructed binary blob"
        case runGetOrFail getFragment (BL.fromStrict payload) of
            Left _ -> Left errInvalidPayload
            Right (_,_,msg) -> case msg of
                Transaction tx -> return (tx, SealedTx payload)
                _ -> Left errInvalidPayload

    -- NOTE
    -- Jörmungandr fee calculation is a linear function where the coefficient
    -- is multiplied by the total number of inputs and outputs.
    , estimateSize = \(CoinSelection inps outs chgs) ->
        Quantity $ length inps + length outs + length chgs

    , estimateMaxNumberOfInputs = \_ _ -> fromIntegral maxNumberOfInputs

    , validateSelection = \(CoinSelection inps outs _) -> do
        when (length inps > maxNumberOfInputs || length outs > maxNumberOfOutputs)
            $ Left ErrExceededInpsOrOuts
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

-- | Provide a transaction witness for a given private key. The type of witness
-- is different between types of keys and, with backward-compatible support, we
-- need to support many types for one backend target.
--
-- We have tightly coupled the type of witness to the type of key for because:
--
-- - ByronKey can only be used with legacy / Byron wallets as they require a
--   special address structure (to embed the derivation path).
--
-- - ShelleyKey could theorically be used with the legacy address structure (as
-- Yoroi does) however, our implementation only associate ShelleyKey to new
-- addresses.
class TxWitnessTagFor (k :: Depth -> * -> *) where
    txWitnessTagFor :: TxWitnessTag

instance TxWitnessTagFor ShelleyKey where txWitnessTagFor = TxWitnessUTxO
instance TxWitnessTagFor ByronKey   where txWitnessTagFor = TxWitnessLegacyUTxO

-- | Transaction with improper number of inputs and outputs is tried
data ErrExceededInpsOrOuts = ErrExceededInpsOrOuts
    deriving (Eq, Show)

instance Buildable ErrExceededInpsOrOuts where
    build _ = build $ mconcat
        [ "I can't validate coin selection because either the number of inputs "
        , "is more than ", maxI," or the number of outputs exceeds ", maxO, "."
        ]
      where
        maxI = toText maxNumberOfInputs
        maxO = toText maxNumberOfOutputs

type instance ErrValidateSelection Jormungandr = ErrExceededInpsOrOuts
