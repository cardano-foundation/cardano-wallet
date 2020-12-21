{-# LANGUAGE DataKinds #-}

module Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxHashSmallRange
    , genTxIndexSmallRange
    , genTxInSmallRange
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
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange, shrinkTokenBundleSmallRange )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn (..), TxOut (..) )
import Data.Either
    ( fromRight )
import Data.Text.Class
    ( FromText (..) )
import Data.Word
    ( Word32 )
import Test.QuickCheck
    ( Gen, elements )
import Test.QuickCheck.Extra
    ( shrinkInterleaved )

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
-- Transaction outputs chosen from a small range (to allow collisions)
--------------------------------------------------------------------------------

genTxOutSmallRange :: Gen TxOut
genTxOutSmallRange = TxOut
    <$> genAddressSmallRange
    <*> genTokenBundleSmallRange

shrinkTxOutSmallRange :: TxOut -> [TxOut]
shrinkTxOutSmallRange (TxOut a b) = uncurry TxOut <$> shrinkInterleaved
    (a, shrinkAddressSmallRange)
    (b, shrinkTokenBundleSmallRange)

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
