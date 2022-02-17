module Cardano.Wallet.Primitive.Types.UTxOIndex.Gen
    ( genUTxOIndex
    , genUTxOIndexLarge
    , genUTxOIndexLargeN
    , shrinkUTxOIndex
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Address.Gen
    ( genAddress, shrinkAddress )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRangePositive, shrinkTokenBundleSmallRangePositive )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn )
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

-- TODO: ADP-1448:
--
-- Replace this type synonym with a type parameter on types that use it.
--
type InputId = (TxIn, Address)

genUTxOIndex :: Gen UTxOIndex
genUTxOIndex = UTxOIndex.fromSequence <$> listOf genEntry
  where
    genEntry :: Gen (InputId, TokenBundle)
    genEntry = (,) <$> genInputId <*> genTokenBundleSmallRangePositive

    genInputId :: Gen InputId
    genInputId = genSized2 genTxIn genAddress

shrinkUTxOIndex :: UTxOIndex -> [UTxOIndex]
shrinkUTxOIndex =
    shrinkMapBy UTxOIndex.fromSequence UTxOIndex.toList (shrinkList shrinkEntry)
  where
    shrinkEntry :: (InputId, TokenBundle) -> [(InputId, TokenBundle)]
    shrinkEntry = genericRoundRobinShrink
        <@> shrinkInputId
        <:> shrinkTokenBundleSmallRangePositive
        <:> Nil

    shrinkInputId :: InputId -> [InputId]
    shrinkInputId = genericRoundRobinShrink
        <@> shrinkTxIn
        <:> shrinkAddress
        <:> Nil

--------------------------------------------------------------------------------
-- Large indices
--------------------------------------------------------------------------------

genUTxOIndexLarge :: Gen UTxOIndex
genUTxOIndexLarge = genUTxOIndexLargeN =<< choose (1024, 4096)

genUTxOIndexLargeN :: Int -> Gen UTxOIndex
genUTxOIndexLargeN n = UTxOIndex.fromSequence <$> replicateM n genEntry
  where
    genEntry :: Gen (InputId, TokenBundle)
    genEntry = (,) <$> genInputId <*> genTokenBundleSmallRangePositive

    genInputId :: Gen InputId
    genInputId = (,) <$> genTxInLargeRange <*> genAddress
