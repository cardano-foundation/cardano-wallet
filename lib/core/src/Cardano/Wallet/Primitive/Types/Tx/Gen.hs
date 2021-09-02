{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTx
    , genTxHash
    , genTxIndex
    , genTxIn
    , genTxInLargeRange
    , genTxOut
    , genScriptValidation
    , shrinkTx
    , shrinkTxHash
    , shrinkTxIndex
    , shrinkTxIn
    , shrinkTxOut
    , shrinkScriptValidation
    )
    where

import Prelude

import Cardano.Wallet.Gen
    ( genNestedTxMetadata, shrinkTxMetadata )
import Cardano.Wallet.Primitive.Types.Address.Gen
    ( genAddress, shrinkAddress )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinPositive, shrinkCoinPositive )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..), mockHash )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount.Gen
    ( genRewardAccount, shrinkRewardAccount )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange, shrinkTokenBundleSmallRange )
import Cardano.Wallet.Primitive.Types.Tx
    ( ScriptValidation (..), Tx (..), TxIn (..), TxMetadata (..), TxOut (..) )
import Control.Monad
    ( replicateM )
import Data.Either
    ( fromRight )
import Data.Map.Strict
    ( Map )
import Data.Text.Class
    ( FromText (..) )
import Data.Word
    ( Word32 )
import Test.QuickCheck
    ( Gen
    , arbitrary
    , elements
    , liftArbitrary
    , liftArbitrary2
    , liftShrink
    , liftShrink2
    , listOf
    , listOf1
    , shrinkList
    , shrinkMapBy
    , sized
    , suchThat
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Extra
    ( genMapWith
    , genSized2With
    , liftShrink7
    , shrinkInterleaved
    , shrinkMapWith
    )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Transactions generated according to the size parameter
--------------------------------------------------------------------------------

genTx :: Gen Tx
genTx = txWithoutIdToTx <$> genTxWithoutId

shrinkTx :: Tx -> [Tx]
shrinkTx = shrinkMapBy txWithoutIdToTx txToTxWithoutId shrinkTxWithoutId

data TxWithoutId = TxWithoutId
    { fee :: !(Maybe Coin)
    , resolvedCollateral :: ![(TxIn, Coin)]
    , resolvedInputs :: ![(TxIn, Coin)]
    , outputs :: ![TxOut]
    , metadata :: !(Maybe TxMetadata)
    , withdrawals :: !(Map RewardAccount Coin)
    , isValidScript :: !ScriptValidation
    }
    deriving (Eq, Ord, Show)

genTxWithoutId :: Gen TxWithoutId
genTxWithoutId = TxWithoutId
    <$> liftArbitrary genCoinPositive
    <*> listOf1 (liftArbitrary2 genTxIn genCoinPositive)
    <*> listOf1 (liftArbitrary2 genTxIn genCoinPositive)
    <*> listOf genTxOut
    <*> liftArbitrary genNestedTxMetadata
    <*> genMapWith genRewardAccount genCoinPositive
    <*> genScriptValidation

shrinkTxWithoutId :: TxWithoutId -> [TxWithoutId]
shrinkTxWithoutId =
    shrinkMapBy tupleToTxWithoutId txWithoutIdToTuple $ liftShrink7
        (liftShrink shrinkCoinPositive)
        (shrinkList (liftShrink2 shrinkTxIn shrinkCoinPositive))
        (shrinkList (liftShrink2 shrinkTxIn shrinkCoinPositive))
        (shrinkList shrinkTxOut)
        (liftShrink shrinkTxMetadata)
        (shrinkMapWith shrinkRewardAccount shrinkCoinPositive)
        shrinkScriptValidation

txWithoutIdToTx :: TxWithoutId -> Tx
txWithoutIdToTx tx@TxWithoutId {..} = Tx {txId = mockHash tx, ..}

txToTxWithoutId :: Tx -> TxWithoutId
txToTxWithoutId Tx {..} = TxWithoutId {..}

txWithoutIdToTuple :: TxWithoutId -> _
txWithoutIdToTuple (TxWithoutId a1 a2 a3 a4 a5 a6 a7) =
    (a1, a2, a3, a4, a5, a6, a7)

tupleToTxWithoutId :: _ -> TxWithoutId
tupleToTxWithoutId (a1, a2, a3, a4, a5, a6, a7) =
    (TxWithoutId a1 a2 a3 a4 a5 a6 a7)

genScriptValidation :: Gen ScriptValidation
genScriptValidation = genericArbitrary

shrinkScriptValidation :: ScriptValidation -> [ScriptValidation]
shrinkScriptValidation = genericShrink

--------------------------------------------------------------------------------
-- Transaction hashes generated according to the size parameter
--------------------------------------------------------------------------------

genTxHash :: Gen (Hash "Tx")
genTxHash = sized $ \size -> elements $ take (max 1 size) txHashes

shrinkTxHash :: Hash "Tx" -> [Hash "Tx"]
shrinkTxHash x
    | x == simplest = []
    | otherwise = [simplest]
  where
    simplest = head txHashes

txHashes :: [Hash "Tx"]
txHashes = mkTxHash <$> ['0' .. '9'] <> ['A' .. 'F']

--------------------------------------------------------------------------------
-- Transaction hashes chosen from a large range (to minimize collisions)
--------------------------------------------------------------------------------

genTxHashLargeRange :: Gen (Hash "Tx")
genTxHashLargeRange = Hash . B8.pack <$> replicateM 32 arbitrary

--------------------------------------------------------------------------------
-- Transaction indices generated according to the size parameter
--------------------------------------------------------------------------------

genTxIndex :: Gen Word32
genTxIndex = sized $ \size -> elements $ take (max 1 size) txIndices

shrinkTxIndex :: Word32 -> [Word32]
shrinkTxIndex 0 = []
shrinkTxIndex _ = [0]

txIndices :: [Word32]
txIndices = [0 ..]

--------------------------------------------------------------------------------
-- Transaction inputs generated according to the size parameter
--------------------------------------------------------------------------------

genTxIn :: Gen TxIn
genTxIn = genSized2With TxIn genTxHash genTxIndex

shrinkTxIn :: TxIn -> [TxIn]
shrinkTxIn (TxIn h i) = uncurry TxIn <$> shrinkInterleaved
    (h, shrinkTxHash)
    (i, shrinkTxIndex)

--------------------------------------------------------------------------------
-- Transaction inputs chosen from a large range (to minimize collisions)
--------------------------------------------------------------------------------

genTxInLargeRange :: Gen TxIn
genTxInLargeRange = TxIn
    <$> genTxHashLargeRange
    -- Note that we don't need to choose indices from a large range, as hashes
    -- are already chosen from a large range:
    <*> genTxIndex

--------------------------------------------------------------------------------
-- Transaction outputs generated according to the size parameter
--------------------------------------------------------------------------------

genTxOut :: Gen TxOut
genTxOut = TxOut
    <$> genAddress
    <*> genTokenBundleSmallRange `suchThat` tokenBundleHasNonZeroCoin

shrinkTxOut :: TxOut -> [TxOut]
shrinkTxOut (TxOut a b) = uncurry TxOut <$> shrinkInterleaved
    (a, shrinkAddress)
    (b, filter tokenBundleHasNonZeroCoin . shrinkTokenBundleSmallRange)

tokenBundleHasNonZeroCoin :: TokenBundle -> Bool
tokenBundleHasNonZeroCoin b = TokenBundle.getCoin b /= Coin 0

--------------------------------------------------------------------------------
-- Internal utilities
--------------------------------------------------------------------------------

-- The input must be a character in the range [0-9] or [A-F].
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
