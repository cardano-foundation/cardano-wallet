{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Primitive.Types.UTxOIndex.Gen
    ( genUTxOIndex
    , genUTxOIndexLarge
    , genUTxOIndexLargeN
    , shrinkUTxOIndex
    ) where

import Prelude

import Cardano.Wallet.CoinSelection
    ( WalletUTxO (..) )
import Cardano.Wallet.Primitive.Types.Address.Gen
    ( genAddress )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRangePositive, shrinkTokenBundleSmallRangePositive )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxInLargeRange )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Control.Monad
    ( replicateM )
import Generics.SOP
    ( NP (..) )
import Test.QuickCheck
    ( Gen, choose, listOf, shrinkList, shrinkMapBy )
import Test.QuickCheck.Extra
    ( genericRoundRobinShrink, (<:>), (<@>) )

import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex

--------------------------------------------------------------------------------
-- Indices generated according to the size parameter
--------------------------------------------------------------------------------

genUTxOIndex :: forall u. Ord u => Gen u -> Gen (UTxOIndex u)
genUTxOIndex genUTxO = UTxOIndex.fromSequence <$> listOf genEntry
  where
    genEntry :: Gen (u, TokenBundle)
    genEntry = (,) <$> genUTxO <*> genTokenBundleSmallRangePositive

shrinkUTxOIndex :: forall u. Ord u => (u -> [u]) -> UTxOIndex u -> [UTxOIndex u]
shrinkUTxOIndex shrinkUTxO =
    shrinkMapBy UTxOIndex.fromSequence UTxOIndex.toList (shrinkList shrinkEntry)
  where
    shrinkEntry :: (u, TokenBundle) -> [(u, TokenBundle)]
    shrinkEntry = genericRoundRobinShrink
        <@> shrinkUTxO
        <:> shrinkTokenBundleSmallRangePositive
        <:> Nil

--------------------------------------------------------------------------------
-- Large indices
--------------------------------------------------------------------------------

genUTxOIndexLarge :: Gen (UTxOIndex WalletUTxO)
genUTxOIndexLarge = genUTxOIndexLargeN =<< choose (1024, 4096)

genUTxOIndexLargeN :: Int -> Gen (UTxOIndex WalletUTxO)
genUTxOIndexLargeN n = UTxOIndex.fromSequence <$> replicateM n genEntry
  where
    genEntry :: Gen (WalletUTxO, TokenBundle)
    genEntry = (,) <$> genWalletUTxO <*> genTokenBundleSmallRangePositive

    genWalletUTxO :: Gen WalletUTxO
    genWalletUTxO = WalletUTxO <$> genTxInLargeRange <*> genAddress
