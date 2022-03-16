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

import Cardano.Wallet.CoinSelection
    ( WalletUTxO (..) )
import Cardano.Wallet.Primitive.Types.Address.Gen
    ( genAddress, shrinkAddress )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxIn, shrinkTxIn )
import Cardano.Wallet.Primitive.Types.UTxOIndex.Gen
    ( genUTxOIndex, shrinkUTxOIndex )
import Cardano.Wallet.Primitive.Types.UTxOSelection
    ( UTxOSelection, UTxOSelectionNonEmpty )
import Data.Maybe
    ( mapMaybe )
import Generics.SOP
    ( NP (..) )
import Test.QuickCheck
    ( Gen, arbitrary, coarbitrary, liftShrink2, shrinkMapBy, suchThatMap )
import Test.QuickCheck.Extra
    ( genFunction, genSized2, genericRoundRobinShrink, (<:>), (<@>) )

import qualified Cardano.Wallet.Primitive.Types.UTxOSelection as UTxOSelection

--------------------------------------------------------------------------------
-- Selections that may be empty
--------------------------------------------------------------------------------

coarbitraryUTxO :: Show u => u -> Gen a -> Gen a
coarbitraryUTxO = coarbitrary . show

genUTxOFunction :: Show u => Gen a -> Gen (u -> a)
genUTxOFunction = genFunction coarbitraryUTxO

genUTxOSelection :: forall u. (Ord u, Show u) => Gen u -> Gen (UTxOSelection u)
genUTxOSelection genUTxO = UTxOSelection.fromIndexFiltered
    <$> genUTxOFilter
    <*> genUTxOIndex genUTxO
  where
    genUTxOFilter :: Gen (u -> Bool)
    genUTxOFilter = genUTxOFunction (arbitrary @Bool)

shrinkUTxOSelection
    :: Ord u => (u -> [u]) -> (UTxOSelection u -> [UTxOSelection u])
shrinkUTxOSelection shrinkUTxO =
    shrinkMapBy UTxOSelection.fromIndexPair UTxOSelection.toIndexPair $
        liftShrink2
            (shrinkUTxOIndex shrinkUTxO)
            (shrinkUTxOIndex shrinkUTxO)

--------------------------------------------------------------------------------
-- Selections that are non-empty
--------------------------------------------------------------------------------

genUTxOSelectionNonEmpty :: Gen (UTxOSelectionNonEmpty WalletUTxO)
genUTxOSelectionNonEmpty =
    genUTxOSelection genWalletUTxO `suchThatMap` UTxOSelection.toNonEmpty
  where
    genWalletUTxO :: Gen WalletUTxO
    genWalletUTxO = uncurry WalletUTxO <$> genSized2 genTxIn genAddress

shrinkUTxOSelectionNonEmpty
    :: UTxOSelectionNonEmpty WalletUTxO
    -> [UTxOSelectionNonEmpty WalletUTxO]
shrinkUTxOSelectionNonEmpty
    = mapMaybe UTxOSelection.toNonEmpty
    . shrinkUTxOSelection shrinkWalletUTxO
    . UTxOSelection.fromNonEmpty
  where
    shrinkWalletUTxO :: WalletUTxO -> [WalletUTxO]
    shrinkWalletUTxO = genericRoundRobinShrink
        <@> shrinkTxIn
        <:> shrinkAddress
        <:> Nil
