{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Types.UTxOSelection.Gen
    ( genUTxOSelection
    , genUTxOSelectionNonEmpty
    , shrinkUTxOSelection
    , shrinkUTxOSelectionNonEmpty
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn )
import Cardano.Wallet.Primitive.Types.UTxOIndex.Gen
    ( genUTxOIndex, shrinkUTxOIndex )
import Cardano.Wallet.Primitive.Types.UTxOSelection
    ( UTxOSelection, UTxOSelectionNonEmpty )
import Data.Maybe
    ( mapMaybe )
import Test.QuickCheck
    ( Gen, arbitrary, coarbitrary, liftShrink2, shrinkMapBy, suchThatMap )
import Test.QuickCheck.Extra
    ( genFunction )

import qualified Cardano.Wallet.Primitive.Types.UTxOSelection as UTxOSelection

--------------------------------------------------------------------------------
-- Selections that may be empty
--------------------------------------------------------------------------------

-- TODO: ADP-1448:
--
-- Replace this type synonym with a type parameter on types that use it.
--
type InputId = (TxIn, Address)

-- TODO: ADP-1448:
--
-- Remove this function once 'InputId' has been replaced with a type parameter.
--
coarbitraryInputId :: InputId -> Gen a -> Gen a
coarbitraryInputId = coarbitrary . show

-- TODO: ADP-1448:
--
-- Remove this function once 'InputId' has been replaced with a type parameter.
--
genInputIdFunction :: Gen a -> Gen (InputId -> a)
genInputIdFunction = genFunction coarbitraryInputId

genUTxOSelection :: Gen (UTxOSelection InputId)
genUTxOSelection = UTxOSelection.fromIndexFiltered
    <$> genFilter
    <*> genUTxOIndex
  where
    genFilter :: Gen (InputId -> Bool)
    genFilter = genInputIdFunction (arbitrary @Bool)

shrinkUTxOSelection :: UTxOSelection InputId -> [UTxOSelection InputId]
shrinkUTxOSelection =
    shrinkMapBy UTxOSelection.fromIndexPair UTxOSelection.toIndexPair $
        liftShrink2
            shrinkUTxOIndex
            shrinkUTxOIndex

--------------------------------------------------------------------------------
-- Selections that are non-empty
--------------------------------------------------------------------------------

genUTxOSelectionNonEmpty :: Gen (UTxOSelectionNonEmpty InputId)
genUTxOSelectionNonEmpty =
    genUTxOSelection `suchThatMap` UTxOSelection.toNonEmpty

shrinkUTxOSelectionNonEmpty
    :: UTxOSelectionNonEmpty InputId
    -> [UTxOSelectionNonEmpty InputId]
shrinkUTxOSelectionNonEmpty
    = mapMaybe UTxOSelection.toNonEmpty
    . shrinkUTxOSelection
    . UTxOSelection.fromNonEmpty

