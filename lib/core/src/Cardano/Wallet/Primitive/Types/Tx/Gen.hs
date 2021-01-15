{-# LANGUAGE DataKinds #-}

module Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxHashSmallRange
    , genTxIndexSmallRange
    , genTxInSmallRange
    , genTxInLargeRange
    , genTxOutSmallRange
    , shrinkTxHashSmallRange
    , shrinkTxIndexSmallRange
    , shrinkTxInSmallRange
    , shrinkTxOutSmallRange
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Types.Address.Gen
    ( genAddressSmallRange, shrinkAddressSmallRange )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange, shrinkTokenBundleSmallRange )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn (..), TxOut (..) )
import Control.Monad
    ( replicateM )
import Data.Either
    ( fromRight )
import Data.Text.Class
    ( FromText (..) )
import Data.Word
    ( Word32 )
import Test.QuickCheck
    ( Gen, arbitrary, elements, suchThat )
import Test.QuickCheck.Extra
    ( shrinkInterleaved )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Transaction hashes chosen from a small range (to allow collisions)
--------------------------------------------------------------------------------

genTxHashSmallRange :: Gen (Hash "Tx")
genTxHashSmallRange = elements txHashes

shrinkTxHashSmallRange :: Hash "Tx" -> [Hash "Tx"]
shrinkTxHashSmallRange hash = filter (< hash) txHashes

txHashes :: [Hash "Tx"]
txHashes = mkTxHash <$> ['0' .. '7']

--------------------------------------------------------------------------------
-- Transaction hashes chosen from a large range (to minimize collisions)
--------------------------------------------------------------------------------

genTxHashLargeRange :: Gen (Hash "Tx")
genTxHashLargeRange = Hash . B8.pack <$> replicateM 32 arbitrary

--------------------------------------------------------------------------------
-- Transaction indices chosen from a small range (to allow collisions)
--------------------------------------------------------------------------------

genTxIndexSmallRange :: Gen Word32
genTxIndexSmallRange = elements txIndices

shrinkTxIndexSmallRange :: Word32 -> [Word32]
shrinkTxIndexSmallRange i = filter (< i) txIndices

txIndices :: [Word32]
txIndices = [0 .. 7]

--------------------------------------------------------------------------------
-- Transaction inputs chosen from a small range (to allow collisions)
--------------------------------------------------------------------------------

genTxInSmallRange :: Gen TxIn
genTxInSmallRange = TxIn
    <$> genTxHashSmallRange
    <*> genTxIndexSmallRange

shrinkTxInSmallRange :: TxIn -> [TxIn]
shrinkTxInSmallRange (TxIn h i) = uncurry TxIn <$> shrinkInterleaved
    (h, shrinkTxHashSmallRange)
    (i, shrinkTxIndexSmallRange)

--------------------------------------------------------------------------------
-- Transaction inputs chosen from a large range (to minimize collisions)
--------------------------------------------------------------------------------

genTxInLargeRange :: Gen TxIn
genTxInLargeRange = TxIn
    <$> genTxHashLargeRange
    -- Note that we don't need to choose indices from a large range, as hashes
    -- are already chosen from a large range:
    <*> genTxIndexSmallRange

--------------------------------------------------------------------------------
-- Transaction outputs chosen from a small range (to allow collisions)
--------------------------------------------------------------------------------

genTxOutSmallRange :: Gen TxOut
genTxOutSmallRange = TxOut
    <$> genAddressSmallRange
    <*> genTokenBundleSmallRange `suchThat` tokenBundleHasNonZeroCoin

shrinkTxOutSmallRange :: TxOut -> [TxOut]
shrinkTxOutSmallRange (TxOut a b) = uncurry TxOut <$> shrinkInterleaved
    (a, shrinkAddressSmallRange)
    (b, filter tokenBundleHasNonZeroCoin . shrinkTokenBundleSmallRange)

tokenBundleHasNonZeroCoin :: TokenBundle -> Bool
tokenBundleHasNonZeroCoin b = TokenBundle.getCoin b /= Coin 0

--------------------------------------------------------------------------------
-- Internal utilities
--------------------------------------------------------------------------------

-- The input must be a character in the range [0-9] or [A-Z].
--
mkTxHash :: Char -> Hash "Tx"
mkTxHash c
    = fromRight reportError
    $ fromText
    $ T.pack
    $ replicate txHashHexStringLength c
  where
    reportError = error $
        "Unable to generate transaction hash from character: " <> show c

txHashHexStringLength :: Int
txHashHexStringLength = 64
