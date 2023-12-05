{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2023 IOHK
-- License: Apache-2.0
--
-- A benchmark for the 'UTxOIndex' type.
--
module Main where

import Prelude

import Cardano.Wallet.Primitive.Types.AssetId
    ( AssetId (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    , mockHash
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..)
    )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap
    )
import Cardano.Wallet.Primitive.Types.TokenName
    ( TokenName (..)
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId (..)
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..)
    )
import Control.DeepSeq
    ( rnf
    )
import Control.Exception
    ( evaluate
    )
import Data.Map.Strict
    ( Map
    )
import Data.Text.Format.Numbers
    ( prettyI
    )
import Numeric.Natural
    ( Natural
    )
import Test.QuickCheck
    ( choose
    , frequency
    , oneof
    , vectorOf
    )
import Test.QuickCheck.Extra
    ( GenSeed (GenSeed)
    , chooseNatural
    , genSizeDefault
    , generateWith
    )
import Test.QuickCheck.Gen
    ( Gen (..)
    )
import Test.Tasty.Bench
    ( Benchmark
    , bench
    , bgroup
    , defaultMain
    , nf
    )

import qualified Cardano.CoinSelection.UTxOIndex as UTxOIndex
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

main :: IO ()
main = benchmarkIndexFromMap

-- | A benchmark for the 'UTxOIndex.fromMap' operation.
--
benchmarkIndexFromMap :: IO ()
benchmarkIndexFromMap = do

    -- Construct UTxO maps of various sizes:

    let utxoMapAdaOnly___1_000 = makeUTxOMapAdaOnly   1_000
    let utxoMapAdaOnly__10_000 = makeUTxOMapAdaOnly  10_000
    let utxoMapAdaOnly_100_000 = makeUTxOMapAdaOnly 100_000

    let utxoMapNFTs___1_000 = makeUTxOMapNFTs   1_000
    let utxoMapNFTs__10_000 = makeUTxOMapNFTs  10_000
    let utxoMapNFTs_100_000 = makeUTxOMapNFTs 100_000

    let utxoMapDiverse___1_000 = makeUTxOMapDiverse   1_000
    let utxoMapDiverse__10_000 = makeUTxOMapDiverse  10_000
    let utxoMapDiverse_100_000 = makeUTxOMapDiverse 100_000

    -- Ensure the UTxO maps are fully evaluated (no thunks):

    evaluate $ rnf utxoMapAdaOnly___1_000
    evaluate $ rnf utxoMapAdaOnly__10_000
    evaluate $ rnf utxoMapAdaOnly_100_000

    evaluate $ rnf utxoMapNFTs___1_000
    evaluate $ rnf utxoMapNFTs__10_000
    evaluate $ rnf utxoMapNFTs_100_000

    evaluate $ rnf utxoMapDiverse___1_000
    evaluate $ rnf utxoMapDiverse__10_000
    evaluate $ rnf utxoMapDiverse_100_000

    defaultMain
        [ bgroup
            "Constructing a UTxO index from a map with ada-only bundles"
            [ makeBenchmark utxoMapAdaOnly___1_000
            , makeBenchmark utxoMapAdaOnly__10_000
            , makeBenchmark utxoMapAdaOnly_100_000
            ]
        , bgroup
            "Constructing a UTxO index from a map with (ada, NFT) bundles"
            [ makeBenchmark utxoMapNFTs___1_000
            , makeBenchmark utxoMapNFTs__10_000
            , makeBenchmark utxoMapNFTs_100_000
            ]
        , bgroup
            "Constructing a UTxO index from a map with diverse bundles"
            [ makeBenchmark utxoMapDiverse___1_000
            , makeBenchmark utxoMapDiverse__10_000
            , makeBenchmark utxoMapDiverse_100_000
            ]
        ]
  where
    makeBenchmark :: UTxOMap -> Benchmark
    makeBenchmark testMap =
        bench description $ nf UTxOIndex.fromMap testMap
      where
        description = unwords
            [ "with"
            , prettyInt (Map.size testMap)
            , "entries"
            ]

type UTxOMap = Map UTxOMapKey TokenBundle
type UTxOMapKey = Natural
type UTxOMapSize = Int

-- | Constructs a map with ada-only token bundles.
--
makeUTxOMapAdaOnly :: UTxOMapSize -> UTxOMap
makeUTxOMapAdaOnly mapSize =
    Map.fromList $ take mapSize $ zip testUTxOMapKeys bundles
  where
    bundles :: [TokenBundle]
    bundles = repeat $ TokenBundle.fromCoin minimalCoin

-- | Constructs a map with bundles consisting of (ada, NFT) pairs.
--
makeUTxOMapNFTs :: UTxOMapSize -> UTxOMap
makeUTxOMapNFTs mapSize =
    Map.fromList $ take mapSize $ zip testUTxOMapKeys bundles
  where
    bundles :: [TokenBundle]
    bundles = makeBundle <$> testUTxOMapKeys

    makeBundle :: Natural -> TokenBundle
    makeBundle n = TokenBundle.fromFlatList minimalCoin
        [(makeTestAssetId n, minimalTokenQuantity)]

-- | Constructs a map with a diverse variety of different bundles.
--
makeUTxOMapDiverse :: UTxOMapSize -> UTxOMap
makeUTxOMapDiverse mapSize =
    -- We use a fixed PRNG seed and QC size parameter to maximise consistency
    -- between benchmark runs:
    generateWith (GenSeed 0) genSizeDefault (genUTxOMapDiverse mapSize)

-- | Generates a map with a diverse variety of different bundles.
--
genUTxOMapDiverse :: UTxOMapSize -> Gen UTxOMap
genUTxOMapDiverse mapSize =
    Map.fromList . zip testUTxOMapKeys <$>
    vectorOf mapSize genTokenBundle
  where
    genTokenBundle :: Gen TokenBundle
    genTokenBundle = TokenBundle <$> genAdaQuantity <*> genTokenMap

    genAdaQuantity :: Gen Coin
    genAdaQuantity = Coin <$> chooseNatural (1, 1_000_000_000)

    genTokenMap :: Gen TokenMap
    genTokenMap = do
        tokenMapSize <- genTokenMapSize
        TokenMap.fromFlatList <$> vectorOf tokenMapSize genAssetQuantity

    genTokenMapSize :: Gen Int
    genTokenMapSize =
        -- Bias toward smaller bundles, but also allow larger bundles:
        frequency
            [ (3, choose (0, 2))
            , (1, choose (3, 8))
            ]

    genAssetQuantity :: Gen (AssetId, TokenQuantity)
    genAssetQuantity = oneof
        [genAssetQuantityFungible, genAssetQuantityNonFungible]

    genAssetQuantityFungible :: Gen (AssetId, TokenQuantity)
    genAssetQuantityFungible =
        (,) <$> genAssetIdFungible <*> genTokenQuantityFungible

    genAssetQuantityNonFungible :: Gen (AssetId, TokenQuantity)
    genAssetQuantityNonFungible =
        (,) <$> genAssetIdNonFungible <*> genTokenQuantityNonFungible

    genAssetIdFungible :: Gen AssetId
    genAssetIdFungible =
        makeTestAssetId <$> chooseNatural (0, 9)

    genAssetIdNonFungible :: Gen AssetId
    genAssetIdNonFungible =
        makeTestAssetId <$> chooseNatural (10, 1_000_000_000)

    genTokenQuantityFungible :: Gen TokenQuantity
    genTokenQuantityFungible =
        TokenQuantity <$> chooseNatural (1, 1_000)

    genTokenQuantityNonFungible :: Gen TokenQuantity
    genTokenQuantityNonFungible =
        pure minimalTokenQuantity

testUTxOMapKeys :: [UTxOMapKey]
testUTxOMapKeys = [0 ..]

makeTestAssetId :: Natural -> AssetId
makeTestAssetId n = AssetId
    (makeTestTokenPolicyId (n `div` 4))
    (makeTestTokenName     (n `mod` 4))

makeTestTokenPolicyId :: Natural -> TokenPolicyId
makeTestTokenPolicyId = UnsafeTokenPolicyId . mockHash

makeTestTokenName :: Natural -> TokenName
makeTestTokenName = UnsafeTokenName . getHash . mockHash

minimalCoin :: Coin
minimalCoin = Coin 1

minimalTokenQuantity :: TokenQuantity
minimalTokenQuantity = TokenQuantity 1

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Pretty-prints a number with separators.
--
-- >>> prettyInt 1_000_000
-- "1,000,000"
--
prettyInt :: Int -> String
prettyInt = T.unpack . prettyI (Just ',')
