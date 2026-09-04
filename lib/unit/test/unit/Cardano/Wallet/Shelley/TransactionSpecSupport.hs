{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Helpers shared by "Cardano.Wallet.Shelley.TransactionSpec" and
-- "Cardano.Wallet.Shelley.TransactionLedgerSpec".
--
-- Both modules drive the same signing properties over a ledger transaction, so
-- they need the same four helpers. They carried a copy each until #5420, which
-- made every change to one of them a change owed to two files.
--
-- This is test support, not a production module: nothing outside the unit test
-- suite may depend on it.
module Cardano.Wallet.Shelley.TransactionSpecSupport
    ( withLedgerTx
    , withSealedLedgerTx
    , setRequiredSigners
    , guardKeyHash
    , checkSubsetOf
    ) where

import Cardano.Address.Derivation
    ( XPrv
    , toXPub
    , xpubPublicKey
    )
import Cardano.Api.Extra
    ( cardanoApiEraConstraints
    , cardanoEraFromRecentEra
    , fromCardanoApiTx
    )
import Cardano.Api.Gen
    ( genTxInEra
    )
import Cardano.Balance.Tx.Eras
    ( RecentEra
    )
import Cardano.Binary.FixedSizeCodec
    ( rawDecodeFixedSized
    )
import Cardano.Ledger.Api
    ( bodyTxL
    )
import Cardano.Ledger.Api.Tx.Body
    ( guardsTxBodyL
    , reqSignerHashesTxBodyL
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..)
    )
import Control.Lens
    ( (.~)
    )
import Data.Ord
    ( comparing
    )
import Fmt
    ( pretty
    )
import Test.QuickCheck
    ( Property
    , checkCoverage
    , counterexample
    , cover
    , forAllBlind
    )
import Prelude

import qualified Cardano.Balance.Tx.Eras as Write
    ( IsRecentEra
    , RecentEra (RecentEraConway, RecentEraDijkstra)
    )
import qualified Cardano.Balance.Tx.Tx as Write
    ( Tx
    )
import qualified Cardano.Ledger.Keys as LedgerKeys
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Wallet.Read as Read
import qualified Data.Foldable as F
import qualified Data.Set as Set
import qualified GHC.Exts as Exts

-- | Generate a ledger transaction in the given era, apply @modifyTx@ to it and
-- hand the result to the property.
withLedgerTx
    :: Write.IsRecentEra era
    => RecentEra era
    -> (Write.Tx era -> Write.Tx era)
    -> (Write.Tx era -> Property)
    -> Property
withLedgerTx recentEra modifyTx cont =
    cardanoApiEraConstraints recentEra
        $ forAllBlind
            ( fromCardanoApiTx
                <$> genTxInEra (cardanoEraFromRecentEra recentEra)
            )
            (cont . modifyTx)

-- | Read a 'SealedTx' back as a ledger transaction of the given era.
withSealedLedgerTx
    :: RecentEra era
    -> SealedTx
    -> (Write.Tx era -> a)
    -> a
withSealedLedgerTx recentEra sealedTx cont = case recentEra of
    Write.RecentEraConway -> case unsafeReadTx sealedTx of
        Read.EraValue (Read.Tx tx :: Read.Tx txEra) -> case Read.theEra @txEra of
            Read.Conway -> cont tx
            _ -> eraMismatch
    Write.RecentEraDijkstra -> case unsafeReadTx sealedTx of
        Read.EraValue (Read.Tx tx :: Read.Tx txEra) -> case Read.theEra @txEra of
            Read.Dijkstra -> cont tx
            _ -> eraMismatch
  where
    eraMismatch = error "withSealedLedgerTx: transaction era mismatch"

-- | Set the transaction's required signers, in whichever field the era keeps
-- them.
setRequiredSigners
    :: RecentEra era
    -> Set.Set (LedgerKeys.KeyHash LedgerKeys.Guard)
    -> Write.Tx era
    -> Write.Tx era
setRequiredSigners recentEra requiredSignerHashes = case recentEra of
    Write.RecentEraConway ->
        bodyTxL . reqSignerHashesTxBodyL .~ requiredSignerHashes
    -- TODO(#5209): the Dijkstra arm below is unverified, and nothing on this
    -- branch can verify it -- the era only activates at the hard fork. Dijkstra
    -- drops @reqSignerHashesTxBodyL@ (it is @notSupportedInThisEraL@ there) and
    -- reaches required signers only through @reqSignerHashesTxBodyG@, a getter
    -- over @guardsTxBodyL@ that keeps @KeyHashObj@ credentials and discards
    -- @ScriptHashObj@ ones; writing @KeyHashObj@ credentials into guards is the
    -- inverse of that getter, which is why it is plausible, not why it is
    -- correct. It is also invisible to the Dijkstra census, whose population is
    -- code that announces itself unimplemented: an @error@ or a @pendingWith@
    -- whose message names the era. A stub fails loudly; this one succeeds, and
    -- would succeed wrongly.
    Write.RecentEraDijkstra ->
        bodyTxL . guardsTxBodyL
            .~ Exts.fromList (SL.KeyHashObj <$> Set.toList requiredSignerHashes)

-- | The ledger key hash a wallet key will sign with, in the role the required
-- signers field wants.
--
-- This is derived the same way 'Cardano.Wallet.Shelley.Transaction.Ledger.mkShelleyWitnessLedger'
-- derives the key of the witness it builds — a fixed-size raw decode over
-- @xpubPublicKey . toXPub@ — so the hash a property declares as required and
-- the hash signing actually produces come from one derivation rather than two.
--
-- 'LedgerKeys.hashKey' is @VKey kd -> KeyHash kd@, so the role is chosen by the
-- return type and no key role is coerced. The previous spelling reached the
-- same bytes through @cardano-api@ and then cast @KeyHash Payment@ to
-- @KeyHash Guard@ with @coerceKeyRole@, which type-checks between any two
-- roles and therefore checked nothing.
guardKeyHash :: XPrv -> LedgerKeys.KeyHash LedgerKeys.Guard
guardKeyHash xprv =
    case rawDecodeFixedSized (xpubPublicKey (toXPub xprv)) of
        Just vk -> LedgerKeys.hashKey (LedgerKeys.VKey vk)
        Nothing -> error "guardKeyHash: invalid public key"

-- | @as \`checkSubsetOf\` bs@ holds when every element of @as@ occurs in @bs@.
--
-- An empty @as@ satisfies that for free, without ever comparing a witness, so
-- a property whose expected set is always empty passes while asserting
-- nothing. The coverage requirement below is what stops that: 'cover' states
-- that the non-vacuous case must be reached, and 'checkCoverage' is what makes
-- the statement fatal rather than advisory — a bare 'cover' only prints a
-- warning, which is a guard that cannot fail.
--
-- The threshold is deliberately low. The claim being enforced is "this
-- comparison is reached with something to compare", not "it usually is".
checkSubsetOf :: (Eq a, Show a) => [a] -> [a] -> Property
checkSubsetOf as bs =
    checkCoverage
        $ cover 10 (not (null as)) "expected set is non-empty"
        $ counterexample counterexampleText
        $ all ((`Set.member` ys) . ShowOrd) as
  where
    xs = Set.fromList (ShowOrd <$> as)
    ys = Set.fromList (ShowOrd <$> bs)

    counterexampleText =
        unlines
            [ "the following set:"
            , showSet xs
            , "is not a subset of:"
            , showSet ys
            , "rogue elements:"
            , showSet (xs `Set.difference` ys)
            ]
      where
        showSet = pretty . fmap (show . unShowOrd) . F.toList

-- | A convenient wrapper type that allows values of any type with a 'Show'
--   instance to be ordered.
newtype ShowOrd a = ShowOrd {unShowOrd :: a}
    deriving (Eq, Show)

instance (Eq a, Show a) => Ord (ShowOrd a) where
    compare = comparing show
