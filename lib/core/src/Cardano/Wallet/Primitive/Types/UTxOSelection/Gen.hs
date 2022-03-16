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

coarbitraryWalletUTxO :: WalletUTxO -> Gen a -> Gen a
coarbitraryWalletUTxO = coarbitrary . show

genWalletUTxOFunction :: Gen a -> Gen (WalletUTxO -> a)
genWalletUTxOFunction = genFunction coarbitraryWalletUTxO

genUTxOSelection :: Gen (UTxOSelection WalletUTxO)
genUTxOSelection = UTxOSelection.fromIndexFiltered
    <$> genFilter
    <*> genUTxOIndex genWalletUTxO
  where
    genFilter :: Gen (WalletUTxO -> Bool)
    genFilter = genWalletUTxOFunction (arbitrary @Bool)

    genWalletUTxO :: Gen WalletUTxO
    genWalletUTxO = uncurry WalletUTxO <$> genSized2 genTxIn genAddress

shrinkUTxOSelection :: UTxOSelection WalletUTxO -> [UTxOSelection WalletUTxO]
shrinkUTxOSelection =
    shrinkMapBy UTxOSelection.fromIndexPair UTxOSelection.toIndexPair $
        liftShrink2
            (shrinkUTxOIndex shrinkWalletUTxO)
            (shrinkUTxOIndex shrinkWalletUTxO)
  where
    shrinkWalletUTxO :: WalletUTxO -> [WalletUTxO]
    shrinkWalletUTxO = genericRoundRobinShrink
        <@> shrinkTxIn
        <:> shrinkAddress
        <:> Nil

--------------------------------------------------------------------------------
-- Selections that are non-empty
--------------------------------------------------------------------------------

genUTxOSelectionNonEmpty :: Gen (UTxOSelectionNonEmpty WalletUTxO)
genUTxOSelectionNonEmpty =
    genUTxOSelection `suchThatMap` UTxOSelection.toNonEmpty

shrinkUTxOSelectionNonEmpty
    :: UTxOSelectionNonEmpty WalletUTxO
    -> [UTxOSelectionNonEmpty WalletUTxO]
shrinkUTxOSelectionNonEmpty
    = mapMaybe UTxOSelection.toNonEmpty
    . shrinkUTxOSelection
    . UTxOSelection.fromNonEmpty
