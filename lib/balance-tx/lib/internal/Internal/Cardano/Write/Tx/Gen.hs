{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
module Internal.Cardano.Write.Tx.Gen
    ( genDatumHash
    , mockPParams
    )
where

import Cardano.Api.Ledger
    ( Coin (..)
    , CostModels
    , PParams
    , PParamsHKD
    , UpgradeConwayPParams (..)
    )
import Cardano.Ledger.Alonzo.PParams
    ( OrdExUnits (..)
    )
import Cardano.Ledger.Babbage.PParams
    ( BabbagePParams (..)
    , CoinPerByte (..)
    )
import Cardano.Ledger.BaseTypes
    ( BoundedRational (..)
    , EpochInterval (..)
    , ProtVer (..)
    , natVersion
    )
import Cardano.Ledger.Conway.PParams
    ( ConwayPParams
    , DRepVotingThresholds (..)
    , PoolVotingThresholds (..)
    , upgradeConwayPParams
    )
import Cardano.Ledger.Plutus
    ( CostModel
    , ExUnits (..)
    , Language (..)
    , Prices (..)
    , mkCostModel
    , mkCostModels
    )
import Data.Functor.Identity
    ( Identity
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Ratio
    ( (%)
    )
import Internal.Cardano.Write.Eras
    ( Babbage
    , Conway
    , IsRecentEra (..)
    , RecentEra (..)
    )
import Internal.Cardano.Write.Tx
    ( DatumHash
    , datumHashFromBytes
    )
import Test.QuickCheck
    ( Gen
    , arbitrary
    , vectorOf
    )
import Unsafe.Coerce
    ( unsafeCoerce
    )
import Prelude

import qualified Data.ByteString as BS
import qualified Data.Map as Map

genDatumHash :: Gen DatumHash
genDatumHash =
    fromMaybe (error "genDatumHash should always generate valid hashes")
        . datumHashFromBytes
        . BS.pack
        <$> vectorOf 32 arbitrary

--------------------------------------------------------------------------------
-- PParams
--------------------------------------------------------------------------------

-- | We try to use similar parameters to mainnet where it matters (in particular
-- fees, execution unit prices, and the cost model.)
mockPParams
    :: forall era. IsRecentEra era => PParams era
mockPParams = case recentEra @era of
    RecentEraBabbage -> unsafeWrap babbagePParams
    RecentEraConway -> unsafeWrap conwayPParams
  where
    -- The constructor of the 'Cardano.Ledger.Core.PParams.PParams' newtype is
    -- not exported.
    unsafeWrap :: PParamsHKD Identity era -> PParams era
    unsafeWrap = unsafeCoerce

    conwayPParams :: ConwayPParams Identity Conway
    conwayPParams = upgradeConwayPParams upgrade babbagePParams
      where
        upgrade :: UpgradeConwayPParams Identity
        upgrade =
            UpgradeConwayPParams
                { ucppPoolVotingThresholds =
                    PoolVotingThresholds
                        { pvtMotionNoConfidence = unsafeBoundRational 0.6
                        , pvtCommitteeNormal = unsafeBoundRational 0.6
                        , pvtCommitteeNoConfidence = unsafeBoundRational 0.51
                        , pvtHardForkInitiation = unsafeBoundRational 0.51
                        , pvtPPSecurityGroup = unsafeBoundRational 0.6
                        }
                , ucppDRepVotingThresholds =
                    DRepVotingThresholds
                        { dvtMotionNoConfidence = unsafeBoundRational 0.67
                        , dvtCommitteeNormal = unsafeBoundRational 0.67
                        , dvtCommitteeNoConfidence = unsafeBoundRational 0.6
                        , dvtUpdateToConstitution = unsafeBoundRational 0.75
                        , dvtHardForkInitiation = unsafeBoundRational 0.6
                        , dvtPPNetworkGroup = unsafeBoundRational 0.67
                        , dvtPPEconomicGroup = unsafeBoundRational 0.67
                        , dvtPPTechnicalGroup = unsafeBoundRational 0.67
                        , dvtPPGovGroup = unsafeBoundRational 0.75
                        , dvtTreasuryWithdrawal = unsafeBoundRational 0.6
                        }
                , ucppCommitteeMinSize = 0
                , ucppCommitteeMaxTermLength = EpochInterval 200
                , ucppGovActionLifetime = EpochInterval 10
                , ucppGovActionDeposit = 1_000 * ada
                , ucppDRepDeposit = 2 * ada
                , ucppDRepActivity = EpochInterval 20
                , ucppMinFeeRefScriptCostPerByte = unsafeBoundRational 44
                , ucppPlutusV3CostModel = conwayPlutusV3CostModel
                }

    babbagePParams :: BabbagePParams Identity Babbage
    babbagePParams =
        BabbagePParams
            { bppMinFeeA = 44
            , -- \^ The linear factor for the minimum fee calculation
              bppMinFeeB = 155_381
            , -- \^ The constant factor for the minimum fee calculation
              bppMaxBBSize = 100_000
            , -- \^ Maximal block body size
              bppMaxTxSize = 16_384
            , -- \^ Maximal transaction size
              bppMaxBHSize = 1_100
            , -- \^ Maximal block header size
              bppKeyDeposit = 2 * ada
            , -- \^ The amount of a key registration deposit
              bppPoolDeposit = 500_000_000
            , -- \^ The amount of a pool registration deposit
              bppEMax = EpochInterval 18
            , -- \^ Maximum number of epochs in the future a pool retirement is allowed to
              -- be scheduled for.
              bppNOpt = 100
            , -- \^ Desired number of pools
              bppA0 = unsafeBoundRational 0.3
            , -- \^ Pool influence
              bppRho = unsafeBoundRational 0.003
            , -- \^ Monetary expansion
              bppTau = unsafeBoundRational 0.2
            , -- \^ Treasury expansion
              bppProtocolVersion = ProtVer (natVersion @6) 0
            , -- \^ Protocol version
              bppMinPoolCost = 170_000_000
            , -- \^ Minimum Stake Pool Cost
              bppCoinsPerUTxOByte = CoinPerByte 4_310
            , -- \^ Cost in lovelace per byte of UTxO storage (instead of bppCoinsPerUTxOByte)
              bppCostModels = babbageCostModels
            , -- \^ Cost models for non-native script languages
              bppPrices =
                Prices
                    (unsafeBoundRational $ 577 % 10_000)
                    (unsafeBoundRational $ 721 % 10_000_000)
            , -- \^ Prices of execution units (for non-native script languages)
              bppMaxTxExUnits = OrdExUnits $ ExUnits 14_000_000 10_000_000_000
            , -- \^ Max total script execution resources units allowed per tx
              bppMaxBlockExUnits = OrdExUnits $ ExUnits 14_000_000 10_000_000_000
            , -- \^ Max total script execution resources units allowed per block
              bppMaxValSize = 5_000
            , -- \^ Max size of a Value in an output
              bppCollateralPercentage = 150
            , -- \^ Percentage of the txfee which must be provided as collateral when
              -- including non-native scripts.
              bppMaxCollateralInputs = 3
            }
    -- \^ Maximum number of collateral inputs allowed in a transaction

    ada :: Coin
    ada = Coin 1_000_000

    unsafeBoundRational :: BoundedRational r => Rational -> r
    unsafeBoundRational x = fromMaybe err $ boundRational x
      where
        err =
            error
                $ unwords
                    [ "unsafeBoundRational:"
                    , show x
                    , "out of bounds"
                    ]

{- HLINT ignore babbageCostModels "Use underscore" -}
babbageCostModels :: CostModels
babbageCostModels = either (error . show) id $ do
    v1 <-
        mkCostModel
            PlutusV1
            [ 197209
            , 0
            , 1
            , 1
            , 396231
            , 621
            , 0
            , 1
            , 150000
            , 1000
            , 0
            , 1
            , 150000
            , 32
            , 2477736
            , 29175
            , 4
            , 29773
            , 100
            , 29773
            , 100
            , 29773
            , 100
            , 29773
            , 100
            , 29773
            , 100
            , 29773
            , 100
            , 100
            , 100
            , 29773
            , 100
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 1000
            , 0
            , 1
            , 150000
            , 32
            , 150000
            , 1000
            , 0
            , 8
            , 148000
            , 425507
            , 118
            , 0
            , 1
            , 1
            , 150000
            , 1000
            , 0
            , 8
            , 150000
            , 112536
            , 247
            , 1
            , 150000
            , 10000
            , 1
            , 136542
            , 1326
            , 1
            , 1000
            , 150000
            , 1000
            , 1
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 1
            , 1
            , 150000
            , 1
            , 150000
            , 4
            , 103599
            , 248
            , 1
            , 103599
            , 248
            , 1
            , 145276
            , 1366
            , 1
            , 179690
            , 497
            , 1
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 148000
            , 425507
            , 118
            , 0
            , 1
            , 1
            , 61516
            , 11218
            , 0
            , 1
            , 150000
            , 32
            , 148000
            , 425507
            , 118
            , 0
            , 1
            , 1
            , 148000
            , 425507
            , 118
            , 0
            , 1
            , 1
            , 2477736
            , 29175
            , 4
            , 0
            , 82363
            , 4
            , 150000
            , 5000
            , 0
            , 1
            , 150000
            , 32
            , 197209
            , 0
            , 1
            , 1
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 150000
            , 32
            , 3345831
            , 1
            , 1
            ]
    v2 <-
        mkCostModel
            PlutusV2
            [ 205665
            , 812
            , 1
            , 1
            , 1000
            , 571
            , 0
            , 1
            , 1000
            , 24177
            , 4
            , 1
            , 1000
            , 32
            , 117366
            , 10475
            , 4
            , 23000
            , 100
            , 23000
            , 100
            , 23000
            , 100
            , 23000
            , 100
            , 23000
            , 100
            , 23000
            , 100
            , 100
            , 100
            , 23000
            , 100
            , 19537
            , 32
            , 175354
            , 32
            , 46417
            , 4
            , 221973
            , 511
            , 0
            , 1
            , 89141
            , 32
            , 497525
            , 14068
            , 4
            , 2
            , 196500
            , 453240
            , 220
            , 0
            , 1
            , 1
            , 1000
            , 28662
            , 4
            , 2
            , 245000
            , 216773
            , 62
            , 1
            , 1060367
            , 12586
            , 1
            , 208512
            , 421
            , 1
            , 187000
            , 1000
            , 52998
            , 1
            , 80436
            , 32
            , 43249
            , 32
            , 1000
            , 32
            , 80556
            , 1
            , 57667
            , 4
            , 1000
            , 10
            , 197145
            , 156
            , 1
            , 197145
            , 156
            , 1
            , 204924
            , 473
            , 1
            , 208896
            , 511
            , 1
            , 52467
            , 32
            , 64832
            , 32
            , 65493
            , 32
            , 22558
            , 32
            , 16563
            , 32
            , 76511
            , 32
            , 196500
            , 453240
            , 220
            , 0
            , 1
            , 1
            , 69522
            , 11687
            , 0
            , 1
            , 60091
            , 32
            , 196500
            , 453240
            , 220
            , 0
            , 1
            , 1
            , 196500
            , 453240
            , 220
            , 0
            , 1
            , 1
            , 1159724
            , 392670
            , 0
            , 2
            , 806990
            , 30482
            , 4
            , 1927926
            , 82523
            , 4
            , 265318
            , 0
            , 4
            , 0
            , 85931
            , 32
            , 205665
            , 812
            , 1
            , 1
            , 41182
            , 32
            , 212342
            , 32
            , 31220
            , 32
            , 32696
            , 32
            , 43357
            , 32
            , 32247
            , 32
            , 38314
            , 32
            , 20000000000
            , 20000000000
            , 9462713
            , 1021
            , 10
            , 20000000000
            , 0
            , 20000000000
            ]
    pure $ mkCostModels $ Map.fromList [(PlutusV1, v1), (PlutusV2, v2)]

{- HLINT ignore conwayPlutusV3CostModel "Use underscore" -}
conwayPlutusV3CostModel :: CostModel
conwayPlutusV3CostModel =
    either (error . show) id
        $ mkCostModel
            PlutusV3
            [ 100788
            , 420
            , 1
            , 1
            , 1000
            , 173
            , 0
            , 1
            , 1000
            , 59957
            , 4
            , 1
            , 11183
            , 32
            , 207616
            , 8310
            , 4
            , 201305
            , 8356
            , 4
            , 962335
            , 18
            , 2780678
            , 6
            , 442008
            , 1
            , 52538055
            , 3756
            , 18
            , 267929
            , 18
            , 76433006
            , 8868
            , 18
            , 52948122
            , 18
            , 1995836
            , 36
            , 3227919
            , 12
            , 901022
            , 1
            , 166917843
            , 4307
            , 36
            , 284546
            , 36
            , 158221314
            , 26549
            , 36
            , 74698472
            , 36
            , 333849714
            , 1
            , 254006273
            , 72
            , 2174038
            , 72
            , 1006041
            , 43623
            , 251
            , 0
            , 1
            , 16000
            , 100
            , 16000
            , 100
            , 16000
            , 100
            , 16000
            , 100
            , 16000
            , 100
            , 16000
            , 100
            , 16000
            , 100
            , 16000
            , 100
            , 100
            , 100
            , 16000
            , 100
            , 94375
            , 32
            , 132994
            , 32
            , 61462
            , 4
            , 72010
            , 178
            , 0
            , 1
            , 22151
            , 32
            , 91189
            , 769
            , 4
            , 2
            , 85848
            , 123203
            , 7305
            , -900
            , 1716
            , 549
            , 57
            , 85848
            , 0
            , 1
            , 1
            , 1000
            , 42921
            , 4
            , 2
            , 24548
            , 29498
            , 38
            , 1
            , 898148
            , 27279
            , 1
            , 51775
            , 558
            , 1
            , 39184
            , 1000
            , 60594
            , 1
            , 141895
            , 32
            , 83150
            , 32
            , 15299
            , 32
            , 76049
            , 1
            , 13169
            , 4
            , 1293828
            , 28716
            , 63
            , 0
            , 1
            , 2261318
            , 64571
            , 4
            , 22100
            , 10
            , 28999
            , 74
            , 1
            , 28999
            , 74
            , 1
            , 43285
            , 552
            , 1
            , 44749
            , 541
            , 1
            , 33852
            , 32
            , 68246
            , 32
            , 72362
            , 32
            , 7243
            , 32
            , 7391
            , 32
            , 11546
            , 32
            , 85848
            , 123203
            , 7305
            , -900
            , 1716
            , 549
            , 57
            , 85848
            , 0
            , 1
            , 90434
            , 519
            , 0
            , 1
            , 74433
            , 32
            , 85848
            , 123203
            , 7305
            , -900
            , 1716
            , 549
            , 57
            , 85848
            , 0
            , 1
            , 1
            , 85848
            , 123203
            , 7305
            , -900
            , 1716
            , 549
            , 57
            , 85848
            , 0
            , 1
            , 955506
            , 213312
            , 0
            , 2
            , 270652
            , 22588
            , 4
            , 1457325
            , 64566
            , 4
            , 20467
            , 1
            , 4
            , 0
            , 141992
            , 32
            , 100788
            , 420
            , 1
            , 1
            , 81663
            , 32
            , 59498
            , 32
            , 20142
            , 32
            , 24588
            , 32
            , 20744
            , 32
            , 25933
            , 32
            , 24623
            , 32
            , 43053543
            , 10
            , 53384111
            , 14333
            , 10
            , 43574283
            , 26308
            , 10
            ]
