{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Types.UTxOSelection.Gen
    ( genUTxOSelection
    , genUTxOSelectionNonEmpty
    , shrinkUTxOSelection
    , shrinkUTxOSelectionNonEmpty
    )
where

import Prelude

import Cardano.Wallet.Primitive.Types.UTxOIndex.Gen
    ( genUTxOIndex
    , shrinkUTxOIndex
    )
import Cardano.Wallet.Primitive.Types.UTxOSelection
    ( UTxOSelection
    , UTxOSelectionNonEmpty
    )
import Data.Maybe
    ( mapMaybe
    )
import Test.QuickCheck
    ( Gen
    , arbitrary
    , coarbitrary
    , liftShrink2
    , shrinkMapBy
    , suchThatMap
    )
import Test.QuickCheck.Extra
    ( genFunction
    )

import qualified Cardano.Wallet.Primitive.Types.UTxOSelection as UTxOSelection

--------------------------------------------------------------------------------
-- Selections that may be empty
--------------------------------------------------------------------------------

coarbitraryUTxO :: Show u => u -> Gen a -> Gen a
coarbitraryUTxO = coarbitrary . show

genUTxOFunction :: Show u => Gen a -> Gen (u -> a)
genUTxOFunction = genFunction coarbitraryUTxO

genUTxOSelection :: forall u. (Ord u, Show u) => Gen u -> Gen (UTxOSelection u)
genUTxOSelection genUTxO =
    UTxOSelection.fromIndexFiltered
        <$> genUTxOFilter
        <*> genUTxOIndex genUTxO
  where
    genUTxOFilter :: Gen (u -> Bool)
    genUTxOFilter = genUTxOFunction (arbitrary @Bool)

shrinkUTxOSelection
    :: Ord u => (u -> [u]) -> (UTxOSelection u -> [UTxOSelection u])
shrinkUTxOSelection shrinkUTxO =
    shrinkMapBy UTxOSelection.fromIndexPair UTxOSelection.toIndexPair
        $ liftShrink2
            (shrinkUTxOIndex shrinkUTxO)
            (shrinkUTxOIndex shrinkUTxO)

--------------------------------------------------------------------------------
-- Selections that are non-empty
--------------------------------------------------------------------------------

genUTxOSelectionNonEmpty
    :: (Ord u, Show u) => Gen u -> Gen (UTxOSelectionNonEmpty u)
genUTxOSelectionNonEmpty genUTxO =
    genUTxOSelection genUTxO `suchThatMap` UTxOSelection.toNonEmpty

shrinkUTxOSelectionNonEmpty
    :: Ord u => (u -> [u]) -> (UTxOSelectionNonEmpty u -> [UTxOSelectionNonEmpty u])
shrinkUTxOSelectionNonEmpty shrinkUTxO =
    mapMaybe UTxOSelection.toNonEmpty
        . shrinkUTxOSelection shrinkUTxO
        . UTxOSelection.fromNonEmpty
