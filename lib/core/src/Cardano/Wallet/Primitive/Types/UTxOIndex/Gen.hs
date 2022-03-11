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
    ( genAddress, shrinkAddress )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRangePositive, shrinkTokenBundleSmallRangePositive )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxIn, genTxInLargeRange, shrinkTxIn )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Control.Monad
    ( replicateM )
import Generics.SOP
    ( NP (..) )
import Test.QuickCheck
    ( Gen, choose, listOf, shrinkList, shrinkMapBy )
import Test.QuickCheck.Extra
    ( genSized2, genericRoundRobinShrink, (<:>), (<@>) )

import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex

--------------------------------------------------------------------------------
-- Indices generated according to the size parameter
--------------------------------------------------------------------------------

genUTxOIndex :: Gen (UTxOIndex WalletUTxO)
genUTxOIndex = UTxOIndex.fromSequence <$> listOf genEntry
  where
    genEntry :: Gen (WalletUTxO, TokenBundle)
    genEntry = (,) <$> genWalletUTxO <*> genTokenBundleSmallRangePositive

    genWalletUTxO :: Gen WalletUTxO
    genWalletUTxO = uncurry WalletUTxO <$> genSized2 genTxIn genAddress

shrinkUTxOIndex :: UTxOIndex WalletUTxO -> [UTxOIndex WalletUTxO]
shrinkUTxOIndex =
    shrinkMapBy UTxOIndex.fromSequence UTxOIndex.toList (shrinkList shrinkEntry)
  where
    shrinkEntry :: (WalletUTxO, TokenBundle) -> [(WalletUTxO, TokenBundle)]
    shrinkEntry = genericRoundRobinShrink
        <@> shrinkWalletUTxO
        <:> shrinkTokenBundleSmallRangePositive
        <:> Nil

    shrinkWalletUTxO :: WalletUTxO -> [WalletUTxO]
    shrinkWalletUTxO = genericRoundRobinShrink
        <@> shrinkTxIn
        <:> shrinkAddress
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
