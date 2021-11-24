{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Cardano.Wallet.Primitive.Types.Tx.Gen
    ( coarbitraryTxIn
    , genTx
    , genTxHash
    , genTxIndex
    , genTxIn
    , genTxInFunction
    , genTxInLargeRange
    , genTxOut
    , genTxOutCoin
    , genTxOutTokenBundle
    , genTxScriptValidity
    , shrinkTx
    , shrinkTxHash
    , shrinkTxIndex
    , shrinkTxIn
    , shrinkTxOut
    , shrinkTxOutCoin
    , shrinkTxScriptValidity
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
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetIdLargeRange )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..)
    , TxIn (..)
    , TxMetadata (..)
    , TxOut (..)
    , TxScriptValidity (..)
    , coinIsValidForTxOut
    , txOutMaxCoin
    , txOutMaxTokenQuantity
    , txOutMinCoin
    , txOutMinTokenQuantity
    )
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
import Generics.SOP
    ( NP (..) )
import GHC.Generics
    ( Generic )
import Test.QuickCheck
    ( Gen
    , arbitrary
    , choose
    , coarbitrary
    , elements
    , frequency
    , liftArbitrary
    , liftArbitrary2
    , liftShrink
    , liftShrink2
    , listOf
    , listOf1
    , oneof
    , shrinkList
    , shrinkMapBy
    , sized
    , suchThat
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Extra
    ( chooseNatural
    , genFunction
    , genMapWith
    , genSized2With
    , genericRoundRobinShrink
    , shrinkInterleaved
    , shrinkMapWith
    , shrinkNatural
    , (<:>)
    , (<@>)
    )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
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
    , scriptValidity :: !(Maybe TxScriptValidity)
    }
    deriving (Eq, Generic, Ord, Show)

genTxWithoutId :: Gen TxWithoutId
genTxWithoutId = TxWithoutId
    <$> liftArbitrary genCoinPositive
    <*> listOf1 (liftArbitrary2 genTxIn genCoinPositive)
    <*> listOf1 (liftArbitrary2 genTxIn genCoinPositive)
    <*> listOf genTxOut
    <*> liftArbitrary genNestedTxMetadata
    <*> genMapWith genRewardAccount genCoinPositive
    <*> liftArbitrary genTxScriptValidity

shrinkTxWithoutId :: TxWithoutId -> [TxWithoutId]
shrinkTxWithoutId = genericRoundRobinShrink
    <@> liftShrink shrinkCoinPositive
    <:> shrinkList (liftShrink2 shrinkTxIn shrinkCoinPositive)
    <:> shrinkList (liftShrink2 shrinkTxIn shrinkCoinPositive)
    <:> shrinkList shrinkTxOut
    <:> liftShrink shrinkTxMetadata
    <:> shrinkMapWith shrinkRewardAccount shrinkCoinPositive
    <:> liftShrink shrinkTxScriptValidity
    <:> Nil

txWithoutIdToTx :: TxWithoutId -> Tx
txWithoutIdToTx tx@TxWithoutId {..} = Tx {txId = mockHash tx, ..}

txToTxWithoutId :: Tx -> TxWithoutId
txToTxWithoutId Tx {..} = TxWithoutId {..}

genTxScriptValidity :: Gen TxScriptValidity
genTxScriptValidity = genericArbitrary

shrinkTxScriptValidity :: TxScriptValidity -> [TxScriptValidity]
shrinkTxScriptValidity = genericShrink

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
-- Transaction input functions
--------------------------------------------------------------------------------

coarbitraryTxIn :: TxIn -> Gen a -> Gen a
coarbitraryTxIn = coarbitrary . show

genTxInFunction :: Gen a -> Gen (TxIn -> a)
genTxInFunction = genFunction coarbitraryTxIn

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
-- Coins chosen from the full range allowed in a transaction output
--------------------------------------------------------------------------------

-- | Generates coins across the full range allowed in a transaction output.
--
-- This generator has a slight bias towards the limits of the range, but
-- otherwise generates values uniformly across the whole range.
--
-- This can be useful when testing roundtrip conversions between different
-- types.
--
genTxOutCoin :: Gen Coin
genTxOutCoin = frequency
    [ (1, pure txOutMinCoin)
    , (1, pure txOutMaxCoin)
    , (8, Coin.fromNatural <$> chooseNatural
        ( Coin.toNatural txOutMinCoin + 1
        , Coin.toNatural txOutMaxCoin - 1
        )
      )
    ]

shrinkTxOutCoin :: Coin -> [Coin]
shrinkTxOutCoin
    = L.filter coinIsValidForTxOut
    . shrinkMapBy Coin.fromNatural Coin.toNatural shrinkNatural

--------------------------------------------------------------------------------
-- Token bundles with fixed numbers of assets.
--
-- Values are chosen from across the full range of values permitted within
-- transaction outputs.
--
-- Policy identifiers, asset names, token quantities are all allowed to vary.
--------------------------------------------------------------------------------

genTxOutTokenBundle :: Int -> Gen TokenBundle
genTxOutTokenBundle fixedAssetCount
    = TokenBundle.fromFlatList
        <$> genTxOutCoin
        <*> replicateM fixedAssetCount genAssetQuantity
  where
    genAssetQuantity = (,)
        <$> genAssetIdLargeRange
        <*> genTokenQuantity
    genTokenQuantity = integerToTokenQuantity <$> oneof
        [ pure $ tokenQuantityToInteger txOutMinTokenQuantity
        , pure $ tokenQuantityToInteger txOutMaxTokenQuantity
        , choose
            ( tokenQuantityToInteger txOutMinTokenQuantity + 1
            , tokenQuantityToInteger txOutMaxTokenQuantity - 1
            )
        ]
      where
        tokenQuantityToInteger :: TokenQuantity -> Integer
        tokenQuantityToInteger = fromIntegral . unTokenQuantity

        integerToTokenQuantity :: Integer -> TokenQuantity
        integerToTokenQuantity = TokenQuantity . fromIntegral

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
