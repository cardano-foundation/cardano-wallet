module Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO
    , genUTxOLarge
    , genUTxOLargeN
    , selectUTxOEntries
    , shrinkUTxO
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn.Gen
    ( genTxIn
    , genTxInLargeRange
    , shrinkTxIn
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen
    ( genTxOut
    , shrinkTxOut
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..)
    )
import Control.Monad
    ( replicateM
    )
import Test.QuickCheck
    ( Gen
    , choose
    , shrinkList
    , sized
    )
import Test.QuickCheck.Extra
    ( selectMapEntries
    , shrinkInterleaved
    )

import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- UTxO sets generated according to the size parameter
--------------------------------------------------------------------------------

genUTxO :: Gen UTxO
genUTxO = sized $ \size -> do
    entryCount <- choose (0, size)
    UTxO . Map.fromList <$> replicateM entryCount genEntry

shrinkUTxO :: UTxO -> [UTxO]
shrinkUTxO
    = take 16
    . fmap (UTxO . Map.fromList)
    . shrinkList shrinkEntry
    . Map.toList
    . unUTxO

genEntry :: Gen (TxIn, TxOut)
genEntry = (,) <$> genTxIn <*> genTxOut

shrinkEntry :: (TxIn, TxOut) -> [(TxIn, TxOut)]
shrinkEntry (i, o) = uncurry (,) <$> shrinkInterleaved
    (i, shrinkTxIn)
    (o, shrinkTxOut)

--------------------------------------------------------------------------------
-- Large UTxO sets
--------------------------------------------------------------------------------

genUTxOLarge :: Gen UTxO
genUTxOLarge = do
    entryCount <- choose (1024, 4096)
    genUTxOLargeN entryCount

genUTxOLargeN :: Int -> Gen UTxO
genUTxOLargeN entryCount = do
    UTxO . Map.fromList <$> replicateM entryCount genEntryLargeRange

genEntryLargeRange :: Gen (TxIn, TxOut)
genEntryLargeRange = (,)
    <$> genTxInLargeRange
    -- Note that we don't need to choose outputs from a large range, as inputs
    -- are already chosen from a large range:
    <*> genTxOut

--------------------------------------------------------------------------------
-- Selecting random UTxO entries
--------------------------------------------------------------------------------

-- | Selects up to a given number of entries at random from the given UTxO set.
--
-- Returns the selected entries and the remaining UTxO set with the entries
-- removed.
--
selectUTxOEntries :: UTxO -> Int -> Gen ([(TxIn, TxOut)], UTxO)
selectUTxOEntries = (fmap (fmap UTxO) .) . selectMapEntries . unUTxO
