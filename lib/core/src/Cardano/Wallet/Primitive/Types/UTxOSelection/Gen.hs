{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Types.UTxOSelection.Gen
    ( genUTxOSelection
    , genUTxOSelectionNonEmpty
    , shrinkUTxOSelection
    , shrinkUTxOSelectionNonEmpty
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( coarbitraryTxIn )
import Cardano.Wallet.Primitive.Types.UTxOIndex.Gen
    ( genUTxOIndex, shrinkUTxOIndex )
import Cardano.Wallet.Primitive.Types.UTxOSelection
    ( UTxOSelection, UTxOSelectionNonEmpty )
import Data.Maybe
    ( mapMaybe )
import Test.QuickCheck
    ( Gen, arbitrary, liftShrink2, shrinkMapBy, suchThatMap )
import Test.QuickCheck.Extra
    ( genFunction )

import qualified Cardano.Wallet.Primitive.Types.UTxOSelection as UTxOSelection

--------------------------------------------------------------------------------
-- Selections that may be empty
--------------------------------------------------------------------------------

genUTxOSelection :: Gen UTxOSelection
genUTxOSelection = UTxOSelection.fromIndexFiltered
    <$> genFilter
    <*> genUTxOIndex
  where
    genFilter :: Gen (TxIn -> Bool)
    genFilter = genFunction coarbitraryTxIn (arbitrary @Bool)

shrinkUTxOSelection :: UTxOSelection -> [UTxOSelection]
shrinkUTxOSelection =
    shrinkMapBy UTxOSelection.fromIndexPair UTxOSelection.toIndexPair $
        liftShrink2
            shrinkUTxOIndex
            shrinkUTxOIndex

--------------------------------------------------------------------------------
-- Selections that are non-empty
--------------------------------------------------------------------------------

genUTxOSelectionNonEmpty :: Gen UTxOSelectionNonEmpty
genUTxOSelectionNonEmpty =
    genUTxOSelection `suchThatMap` UTxOSelection.toNonEmpty

shrinkUTxOSelectionNonEmpty :: UTxOSelectionNonEmpty -> [UTxOSelectionNonEmpty]
shrinkUTxOSelectionNonEmpty
    = mapMaybe UTxOSelection.toNonEmpty
    . shrinkUTxOSelection
    . UTxOSelection.fromNonEmpty

