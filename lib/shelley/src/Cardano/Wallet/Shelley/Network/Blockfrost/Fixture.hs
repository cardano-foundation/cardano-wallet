{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Shelley.Network.Blockfrost.Fixture where

import Prelude

import Cardano.Api
    ( AnyCardanoEra (..)
    , CardanoEra (AllegraEra, AlonzoEra, ByronEra, MaryEra, ShelleyEra)
    , CostModel (..)
    , EpochNo (EpochNo)
    , NetworkId (..)
    , NetworkMagic (..)
    )
import Cardano.Slotting.Slot
    ( EpochSize (..), SlotNo (..) )
import Cardano.Slotting.Time
    ( RelativeTime (..), mkSlotLength )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..) )
import Data.Functor
    ( (<&>) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( fromText )
import Ouroboros.Consensus.HardFork.History
    ( Bound (Bound, boundEpoch, boundSlot, boundTime)
    , EraEnd (EraEnd, EraUnbounded)
    , EraParams (EraParams, eraEpochSize, eraSafeZone, eraSlotLength)
    , EraSummary (EraSummary, eraEnd, eraParams, eraStart)
    , SafeZone (StandardSafeZone)
    , Summary (..)
    )

import qualified Cardano.Wallet.Primitive.Types as W
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash )
import Data.Either
    ( fromRight )
import qualified Data.Map as Map
import qualified Ouroboros.Consensus.Cardano.Block as OC
import qualified Ouroboros.Consensus.Util.Counting as OCC

costModels :: NetworkId -> CostModel
costModels = \case
    Mainnet -> CostModel $ Map.fromList
        [ ("sha2_256-memory-arguments", 4)
        , ("equalsString-cpu-arguments-constant", 1000)
        , ("cekDelayCost-exBudgetMemory", 100)
        , ("lessThanEqualsByteString-cpu-arguments-intercept", 103599)
        , ("divideInteger-memory-arguments-minimum", 1)
        , ("appendByteString-cpu-arguments-slope", 621)
        , ("blake2b-cpu-arguments-slope", 29175)
        , ("iData-cpu-arguments", 150000)
        , ("encodeUtf8-cpu-arguments-slope", 1000)
        , ("unBData-cpu-arguments", 150000)
        , ("multiplyInteger-cpu-arguments-intercept", 61516)
        , ("cekConstCost-exBudgetMemory", 100)
        , ("nullList-cpu-arguments", 150000)
        , ("equalsString-cpu-arguments-intercept", 150000)
        , ("trace-cpu-arguments", 150000)
        , ("mkNilData-memory-arguments", 32)
        , ("lengthOfByteString-cpu-arguments", 150000)
        , ("cekBuiltinCost-exBudgetCPU", 29773)
        , ("bData-cpu-arguments", 150000)
        , ("subtractInteger-cpu-arguments-slope", 0)
        , ("unIData-cpu-arguments", 150000)
        , ("consByteString-memory-arguments-intercept", 0)
        , ("divideInteger-memory-arguments-slope", 1)
        , ("divideInteger-cpu-arguments-model-arguments-slope", 118)
        , ("listData-cpu-arguments", 150000)
        , ("headList-cpu-arguments", 150000)
        , ("chooseData-memory-arguments", 32)
        , ("equalsInteger-cpu-arguments-intercept", 136542)
        , ("sha3_256-cpu-arguments-slope", 82363)
        , ("sliceByteString-cpu-arguments-slope", 5000)
        , ("unMapData-cpu-arguments", 150000)
        , ("lessThanInteger-cpu-arguments-intercept", 179690)
        , ("mkCons-cpu-arguments", 150000)
        , ("appendString-memory-arguments-intercept", 0)
        , ("modInteger-cpu-arguments-model-arguments-slope", 118)
        , ("ifThenElse-cpu-arguments", 1)
        , ("mkNilPairData-cpu-arguments", 150000)
        , ("lessThanEqualsInteger-cpu-arguments-intercept", 145276)
        , ("addInteger-memory-arguments-slope", 1)
        , ("chooseList-memory-arguments", 32)
        , ("constrData-memory-arguments", 32)
        , ("decodeUtf8-cpu-arguments-intercept", 150000)
        , ("equalsData-memory-arguments", 1)
        , ("subtractInteger-memory-arguments-slope", 1)
        , ("appendByteString-memory-arguments-intercept", 0)
        , ("lengthOfByteString-memory-arguments", 4)
        , ("headList-memory-arguments", 32)
        , ("listData-memory-arguments", 32)
        , ("consByteString-cpu-arguments-intercept", 150000)
        , ("unIData-memory-arguments", 32)
        , ("remainderInteger-memory-arguments-minimum", 1)
        , ("bData-memory-arguments", 32)
        , ("lessThanByteString-cpu-arguments-slope", 248)
        , ("encodeUtf8-memory-arguments-intercept", 0)
        , ("cekStartupCost-exBudgetCPU", 100)
        , ("multiplyInteger-memory-arguments-intercept", 0)
        , ("unListData-memory-arguments", 32)
        , ("remainderInteger-cpu-arguments-model-arguments-slope", 118)
        , ("cekVarCost-exBudgetCPU", 29773)
        , ("remainderInteger-memory-arguments-slope", 1)
        , ("cekForceCost-exBudgetCPU", 29773)
        , ("sha2_256-cpu-arguments-slope", 29175)
        , ("equalsInteger-memory-arguments", 1)
        , ("indexByteString-memory-arguments", 1)
        , ("addInteger-memory-arguments-intercept", 1)
        , ("chooseUnit-cpu-arguments", 150000)
        , ("sndPair-cpu-arguments", 150000)
        , ("cekLamCost-exBudgetCPU", 29773)
        , ("fstPair-cpu-arguments", 150000)
        , ("quotientInteger-memory-arguments-minimum", 1)
        , ("decodeUtf8-cpu-arguments-slope", 1000)
        , ("lessThanInteger-memory-arguments", 1)
        , ("lessThanEqualsInteger-cpu-arguments-slope", 1366)
        , ("fstPair-memory-arguments", 32)
        , ("modInteger-memory-arguments-intercept", 0)
        , ("unConstrData-cpu-arguments", 150000)
        , ("lessThanEqualsInteger-memory-arguments", 1)
        , ("chooseUnit-memory-arguments", 32)
        , ("sndPair-memory-arguments", 32)
        , ("addInteger-cpu-arguments-intercept", 197209)
        , ("decodeUtf8-memory-arguments-slope", 8)
        , ("equalsData-cpu-arguments-intercept", 150000)
        , ("mapData-cpu-arguments", 150000)
        , ("mkPairData-cpu-arguments", 150000)
        , ("quotientInteger-cpu-arguments-constant", 148000)
        , ("consByteString-memory-arguments-slope", 1)
        , ("cekVarCost-exBudgetMemory", 100)
        , ("indexByteString-cpu-arguments", 150000)
        , ("unListData-cpu-arguments", 150000)
        , ("equalsInteger-cpu-arguments-slope", 1326)
        , ("cekStartupCost-exBudgetMemory", 100)
        , ("subtractInteger-cpu-arguments-intercept", 197209)
        , ("divideInteger-cpu-arguments-model-arguments-intercept", 425507)
        , ("divideInteger-memory-arguments-intercept", 0)
        , ("cekForceCost-exBudgetMemory", 100)
        , ("blake2b-cpu-arguments-intercept", 2477736)
        , ("remainderInteger-cpu-arguments-constant", 148000)
        , ("tailList-cpu-arguments", 150000)
        , ("encodeUtf8-cpu-arguments-intercept", 150000)
        , ("equalsString-cpu-arguments-slope", 1000)
        , ("lessThanByteString-memory-arguments", 1)
        , ("multiplyInteger-cpu-arguments-slope", 11218)
        , ("appendByteString-cpu-arguments-intercept", 396231)
        , ("lessThanEqualsByteString-cpu-arguments-slope", 248)
        , ("modInteger-memory-arguments-slope", 1)
        , ("addInteger-cpu-arguments-slope", 0)
        , ("equalsData-cpu-arguments-slope", 10000)
        , ("decodeUtf8-memory-arguments-intercept", 0)
        , ("chooseList-cpu-arguments", 150000)
        , ("constrData-cpu-arguments", 150000)
        , ("equalsByteString-memory-arguments", 1)
        , ("cekApplyCost-exBudgetCPU", 29773)
        , ("quotientInteger-memory-arguments-slope", 1)
        , ("verifySignature-cpu-arguments-intercept", 3345831)
        , ("unMapData-memory-arguments", 32)
        , ("mkCons-memory-arguments", 32)
        , ("sliceByteString-memory-arguments-slope", 1)
        , ("sha3_256-memory-arguments", 4)
        , ("ifThenElse-memory-arguments", 1)
        , ("mkNilPairData-memory-arguments", 32)
        , ("equalsByteString-cpu-arguments-slope", 247)
        , ("appendString-cpu-arguments-intercept", 150000)
        , ("quotientInteger-cpu-arguments-model-arguments-slope", 118)
        , ("cekApplyCost-exBudgetMemory", 100)
        , ("equalsString-memory-arguments", 1)
        , ("multiplyInteger-memory-arguments-slope", 1)
        , ("cekBuiltinCost-exBudgetMemory", 100)
        , ("remainderInteger-memory-arguments-intercept", 0)
        , ("sha2_256-cpu-arguments-intercept", 2477736)
        , ("remainderInteger-cpu-arguments-model-arguments-intercept", 425507)
        , ("lessThanEqualsByteString-memory-arguments", 1)
        , ("tailList-memory-arguments", 32)
        , ("mkNilData-cpu-arguments", 150000)
        , ("chooseData-cpu-arguments", 150000)
        , ("unBData-memory-arguments", 32)
        , ("blake2b-memory-arguments", 4)
        , ("iData-memory-arguments", 32)
        , ("nullList-memory-arguments", 32)
        , ("cekDelayCost-exBudgetCPU", 29773)
        , ("subtractInteger-memory-arguments-intercept", 1)
        , ("lessThanByteString-cpu-arguments-intercept", 103599)
        , ("consByteString-cpu-arguments-slope", 1000)
        , ("appendByteString-memory-arguments-slope", 1)
        , ("trace-memory-arguments", 32)
        , ("divideInteger-cpu-arguments-constant", 148000)
        , ("cekConstCost-exBudgetCPU", 29773)
        , ("encodeUtf8-memory-arguments-slope", 8)
        , ("quotientInteger-cpu-arguments-model-arguments-intercept", 425507)
        , ("mapData-memory-arguments", 32)
        , ("appendString-cpu-arguments-slope", 1000)
        , ("modInteger-cpu-arguments-constant", 148000)
        , ("verifySignature-cpu-arguments-slope", 1)
        , ("unConstrData-memory-arguments", 32)
        , ("quotientInteger-memory-arguments-intercept", 0)
        , ("equalsByteString-cpu-arguments-constant", 150000)
        , ("sliceByteString-memory-arguments-intercept", 0)
        , ("mkPairData-memory-arguments", 32)
        , ("equalsByteString-cpu-arguments-intercept", 112536)
        , ("appendString-memory-arguments-slope", 1)
        , ("lessThanInteger-cpu-arguments-slope", 497)
        , ("modInteger-cpu-arguments-model-arguments-intercept", 425507)
        , ("modInteger-memory-arguments-minimum", 1)
        , ("sha3_256-cpu-arguments-intercept", 0)
        , ("verifySignature-memory-arguments", 1)
        , ("cekLamCost-exBudgetMemory", 100)
        , ("sliceByteString-cpu-arguments-intercept", 15000)
        ]
    Testnet (NetworkMagic 1097911063) -> CostModel $ Map.fromList
        [ ("sha2_256-memory-arguments", 4)
        , ("equalsString-cpu-arguments-constant", 1000)
        , ("cekDelayCost-exBudgetMemory", 100)
        , ("lessThanEqualsByteString-cpu-arguments-intercept", 103599)
        , ("divideInteger-memory-arguments-minimum", 1)
        , ("appendByteString-cpu-arguments-slope", 621)
        , ("blake2b-cpu-arguments-slope", 29175)
        , ("iData-cpu-arguments", 150000)
        , ("encodeUtf8-cpu-arguments-slope", 1000)
        , ("unBData-cpu-arguments", 150000)
        , ("multiplyInteger-cpu-arguments-intercept", 61516)
        , ("cekConstCost-exBudgetMemory", 100)
        , ("nullList-cpu-arguments", 150000)
        , ("equalsString-cpu-arguments-intercept", 150000)
        , ("trace-cpu-arguments", 150000)
        , ("mkNilData-memory-arguments", 32)
        , ("lengthOfByteString-cpu-arguments", 150000)
        , ("cekBuiltinCost-exBudgetCPU", 29773)
        , ("bData-cpu-arguments", 150000)
        , ("subtractInteger-cpu-arguments-slope", 0)
        , ("unIData-cpu-arguments", 150000)
        , ("consByteString-memory-arguments-intercept", 0)
        , ("divideInteger-memory-arguments-slope", 1)
        , ("divideInteger-cpu-arguments-model-arguments-slope", 118)
        , ("listData-cpu-arguments", 150000)
        , ("headList-cpu-arguments", 150000)
        , ("chooseData-memory-arguments", 32)
        , ("equalsInteger-cpu-arguments-intercept", 136542)
        , ("sha3_256-cpu-arguments-slope", 82363)
        , ("sliceByteString-cpu-arguments-slope", 5000)
        , ("unMapData-cpu-arguments", 150000)
        , ("lessThanInteger-cpu-arguments-intercept", 179690)
        , ("mkCons-cpu-arguments", 150000)
        , ("appendString-memory-arguments-intercept", 0)
        , ("modInteger-cpu-arguments-model-arguments-slope", 118)
        , ("ifThenElse-cpu-arguments", 1)
        , ("mkNilPairData-cpu-arguments", 150000)
        , ("lessThanEqualsInteger-cpu-arguments-intercept", 145276)
        , ("addInteger-memory-arguments-slope", 1)
        , ("chooseList-memory-arguments", 32)
        , ("constrData-memory-arguments", 32)
        , ("decodeUtf8-cpu-arguments-intercept", 150000)
        , ("equalsData-memory-arguments", 1)
        , ("subtractInteger-memory-arguments-slope", 1)
        , ("appendByteString-memory-arguments-intercept", 0)
        , ("lengthOfByteString-memory-arguments", 4)
        , ("headList-memory-arguments", 32)
        , ("listData-memory-arguments", 32)
        , ("consByteString-cpu-arguments-intercept", 150000)
        , ("unIData-memory-arguments", 32)
        , ("remainderInteger-memory-arguments-minimum", 1)
        , ("bData-memory-arguments", 32)
        , ("lessThanByteString-cpu-arguments-slope", 248)
        , ("encodeUtf8-memory-arguments-intercept", 0)
        , ("cekStartupCost-exBudgetCPU", 100)
        , ("multiplyInteger-memory-arguments-intercept", 0)
        , ("unListData-memory-arguments", 32)
        , ("remainderInteger-cpu-arguments-model-arguments-slope", 118)
        , ("cekVarCost-exBudgetCPU", 29773)
        , ("remainderInteger-memory-arguments-slope", 1)
        , ("cekForceCost-exBudgetCPU", 29773)
        , ("sha2_256-cpu-arguments-slope", 29175)
        , ("equalsInteger-memory-arguments", 1)
        , ("indexByteString-memory-arguments", 1)
        , ("addInteger-memory-arguments-intercept", 1)
        , ("chooseUnit-cpu-arguments", 150000)
        , ("sndPair-cpu-arguments", 150000)
        , ("cekLamCost-exBudgetCPU", 29773)
        , ("fstPair-cpu-arguments", 150000)
        , ("quotientInteger-memory-arguments-minimum", 1)
        , ("decodeUtf8-cpu-arguments-slope", 1000)
        , ("lessThanInteger-memory-arguments", 1)
        , ("lessThanEqualsInteger-cpu-arguments-slope", 1366)
        , ("fstPair-memory-arguments", 32)
        , ("modInteger-memory-arguments-intercept", 0)
        , ("unConstrData-cpu-arguments", 150000)
        , ("lessThanEqualsInteger-memory-arguments", 1)
        , ("chooseUnit-memory-arguments", 32)
        , ("sndPair-memory-arguments", 32)
        , ("addInteger-cpu-arguments-intercept", 197209)
        , ("decodeUtf8-memory-arguments-slope", 8)
        , ("equalsData-cpu-arguments-intercept", 150000)
        , ("mapData-cpu-arguments", 150000)
        , ("mkPairData-cpu-arguments", 150000)
        , ("quotientInteger-cpu-arguments-constant", 148000)
        , ("consByteString-memory-arguments-slope", 1)
        , ("cekVarCost-exBudgetMemory", 100)
        , ("indexByteString-cpu-arguments", 150000)
        , ("unListData-cpu-arguments", 150000)
        , ("equalsInteger-cpu-arguments-slope", 1326)
        , ("cekStartupCost-exBudgetMemory", 100)
        , ("subtractInteger-cpu-arguments-intercept", 197209)
        , ("divideInteger-cpu-arguments-model-arguments-intercept", 425507)
        , ("divideInteger-memory-arguments-intercept", 0)
        , ("cekForceCost-exBudgetMemory", 100)
        , ("blake2b-cpu-arguments-intercept", 2477736)
        , ("remainderInteger-cpu-arguments-constant", 148000)
        , ("tailList-cpu-arguments", 150000)
        , ("encodeUtf8-cpu-arguments-intercept", 150000)
        , ("equalsString-cpu-arguments-slope", 1000)
        , ("lessThanByteString-memory-arguments", 1)
        , ("multiplyInteger-cpu-arguments-slope", 11218)
        , ("appendByteString-cpu-arguments-intercept", 396231)
        , ("lessThanEqualsByteString-cpu-arguments-slope", 248)
        , ("modInteger-memory-arguments-slope", 1)
        , ("addInteger-cpu-arguments-slope", 0)
        , ("equalsData-cpu-arguments-slope", 10000)
        , ("decodeUtf8-memory-arguments-intercept", 0)
        , ("chooseList-cpu-arguments", 150000)
        , ("constrData-cpu-arguments", 150000)
        , ("equalsByteString-memory-arguments", 1)
        , ("cekApplyCost-exBudgetCPU", 29773)
        , ("quotientInteger-memory-arguments-slope", 1)
        , ("verifySignature-cpu-arguments-intercept", 3345831)
        , ("unMapData-memory-arguments", 32)
        , ("mkCons-memory-arguments", 32)
        , ("sliceByteString-memory-arguments-slope", 1)
        , ("sha3_256-memory-arguments", 4)
        , ("ifThenElse-memory-arguments", 1)
        , ("mkNilPairData-memory-arguments", 32)
        , ("equalsByteString-cpu-arguments-slope", 247)
        , ("appendString-cpu-arguments-intercept", 150000)
        , ("quotientInteger-cpu-arguments-model-arguments-slope", 118)
        , ("cekApplyCost-exBudgetMemory", 100)
        , ("equalsString-memory-arguments", 1)
        , ("multiplyInteger-memory-arguments-slope", 1)
        , ("cekBuiltinCost-exBudgetMemory", 100)
        , ("remainderInteger-memory-arguments-intercept", 0)
        , ("sha2_256-cpu-arguments-intercept", 2477736)
        , ("remainderInteger-cpu-arguments-model-arguments-intercept", 425507)
        , ("lessThanEqualsByteString-memory-arguments", 1)
        , ("tailList-memory-arguments", 32)
        , ("mkNilData-cpu-arguments", 150000)
        , ("chooseData-cpu-arguments", 150000)
        , ("unBData-memory-arguments", 32)
        , ("blake2b-memory-arguments", 4)
        , ("iData-memory-arguments", 32)
        , ("nullList-memory-arguments", 32)
        , ("cekDelayCost-exBudgetCPU", 29773)
        , ("subtractInteger-memory-arguments-intercept", 1)
        , ("lessThanByteString-cpu-arguments-intercept", 103599)
        , ("consByteString-cpu-arguments-slope", 1000)
        , ("appendByteString-memory-arguments-slope", 1)
        , ("trace-memory-arguments", 32)
        , ("divideInteger-cpu-arguments-constant", 148000)
        , ("cekConstCost-exBudgetCPU", 29773)
        , ("encodeUtf8-memory-arguments-slope", 8)
        , ("quotientInteger-cpu-arguments-model-arguments-intercept", 425507)
        , ("mapData-memory-arguments", 32)
        , ("appendString-cpu-arguments-slope", 1000)
        , ("modInteger-cpu-arguments-constant", 148000)
        , ("verifySignature-cpu-arguments-slope", 1)
        , ("unConstrData-memory-arguments", 32)
        , ("quotientInteger-memory-arguments-intercept", 0)
        , ("equalsByteString-cpu-arguments-constant", 150000)
        , ("sliceByteString-memory-arguments-intercept", 0)
        , ("mkPairData-memory-arguments", 32)
        , ("equalsByteString-cpu-arguments-intercept", 112536)
        , ("appendString-memory-arguments-slope", 1)
        , ("lessThanInteger-cpu-arguments-slope", 497)
        , ("modInteger-cpu-arguments-model-arguments-intercept", 425507)
        , ("modInteger-memory-arguments-minimum", 1)
        , ("sha3_256-cpu-arguments-intercept", 0)
        , ("verifySignature-memory-arguments", 1)
        , ("cekLamCost-exBudgetMemory", 100)
        , ("sliceByteString-cpu-arguments-intercept", 150000)
        ]
    Testnet magic ->
        error $ "CostModel isn't provided for the Testnet " <> show magic

genesisBlockHeaderHash :: NetworkId -> Hash "BlockHeader"
genesisBlockHeaderHash = \case
    Mainnet -> unsafeHash
        "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"
    Testnet (NetworkMagic 1097911063) -> unsafeHash
        "96fceff972c2c06bd3bb5243c39215333be6d56aaf4823073dca31afe5038471"
    Testnet m -> error $
        "Genesis block header hash isn't provided for the Testnet " <> show m
  where
    unsafeHash h = fromRight (error $ "Invalid hash: " <> show h) $ fromText h

genesisBlockHeader :: NetworkId -> BlockHeader
genesisBlockHeader network =
    BlockHeader
        { slotNo = 0
        , blockHeight = Quantity 0
        , headerHash = genesisBlockHeaderHash network
        , parentHeaderHash = Nothing
        }

networkSummary :: NetworkId -> Summary (OC.CardanoEras OC.StandardCrypto)
networkSummary = \case
    Mainnet ->
        Summary
        { getSummary =
            -- Byron
            OCC.NonEmptyCons
                EraSummary
                { eraStart =
                    Bound
                    { boundTime = RelativeTime 0
                    , boundSlot = 0
                    , boundEpoch = 0
                    }
                , eraEnd =
                    EraEnd Bound
                    { boundTime = RelativeTime 89856000
                    , boundSlot = 4492800
                    , boundEpoch = EpochNo 208
                    }
                , eraParams =
                    EraParams
                    { eraEpochSize = EpochSize 21600
                    , eraSlotLength = mkSlotLength 20
                    , eraSafeZone = StandardSafeZone 4320
                    }
                }
                -- Shelley
                $ OCC.NonEmptyCons
                    EraSummary
                    { eraStart =
                        Bound
                        { boundTime = RelativeTime 89856000
                        , boundSlot = 4492800
                        , boundEpoch = EpochNo 208
                        }
                    , eraEnd =
                        EraEnd Bound
                        { boundTime = RelativeTime 101952000
                        , boundSlot = 16588800
                        , boundEpoch = EpochNo 236
                        }
                    , eraParams =
                        EraParams
                        { eraEpochSize = EpochSize 432000
                        , eraSlotLength = mkSlotLength 1
                        , eraSafeZone = StandardSafeZone 129600
                        }
                    }
                    -- Allegra
                    $ OCC.NonEmptyCons
                        EraSummary
                        { eraStart =
                            Bound
                            { boundTime = RelativeTime 101952000
                            , boundSlot = 16588800
                            , boundEpoch = EpochNo 236
                            }
                        , eraEnd =
                            EraEnd Bound
                            { boundTime = RelativeTime 108432000
                            , boundSlot = 23068800
                            , boundEpoch = EpochNo 251
                            }
                        , eraParams =
                            EraParams
                            { eraEpochSize = EpochSize 432000
                            , eraSlotLength = mkSlotLength 1
                            , eraSafeZone = StandardSafeZone 129600
                            }
                        }
                        -- Mary
                        $ OCC.NonEmptyCons
                            EraSummary
                            { eraStart =
                                Bound
                                { boundTime = RelativeTime 108432000
                                , boundSlot = 23068800
                                , boundEpoch = EpochNo 251
                                }
                            , eraEnd =
                                EraEnd Bound
                                { boundTime = RelativeTime 125280000
                                , boundSlot = 39916800
                                , boundEpoch = EpochNo 290
                                }
                            , eraParams =
                                EraParams
                                { eraEpochSize = EpochSize 432000
                                , eraSlotLength = mkSlotLength 1
                                , eraSafeZone = StandardSafeZone 129600
                                }
                            }
                            -- Alonzo
                            $ OCC.NonEmptyOne
                                EraSummary
                                { eraStart =
                                    Bound
                                    { boundTime = RelativeTime 125280000
                                    , boundSlot = 39916800
                                    , boundEpoch = EpochNo 290
                                    }
                                , eraEnd = EraUnbounded
                                , eraParams =
                                    EraParams
                                    { eraEpochSize = EpochSize 432000
                                    , eraSlotLength = mkSlotLength 1
                                    , eraSafeZone = StandardSafeZone 129600
                                    }
                                }
        }
    Testnet (NetworkMagic 1097911063) ->
        -- Magic of the current public testnet
        Summary
        { getSummary =
            OCC.NonEmptyCons
                EraSummary
                { eraStart =
                    Bound
                    { boundTime = RelativeTime 0
                    , boundSlot = SlotNo 0
                    , boundEpoch = EpochNo 0
                    }
                , eraEnd =
                    EraEnd Bound
                    { boundTime = RelativeTime 31968000
                    , boundSlot = SlotNo 1598400
                    , boundEpoch = EpochNo 74
                    }
                , eraParams =
                    EraParams
                    { eraEpochSize = EpochSize 21600
                    , eraSlotLength = mkSlotLength 20
                    , eraSafeZone = StandardSafeZone 4320
                    }
                }
                $ OCC.NonEmptyCons
                    EraSummary
                    { eraStart =
                        Bound
                        { boundTime = RelativeTime 31968000
                        , boundSlot = SlotNo 1598400
                        , boundEpoch = EpochNo 74
                        }
                    , eraEnd =
                        EraEnd Bound
                        { boundTime = RelativeTime 44064000
                        , boundSlot = SlotNo 13694400
                        , boundEpoch = EpochNo 102
                        }
                    , eraParams =
                        EraParams
                        { eraEpochSize = EpochSize 432000
                        , eraSlotLength = mkSlotLength 1
                        , eraSafeZone = StandardSafeZone 129600
                        }
                    }
                    $ OCC.NonEmptyCons
                        EraSummary
                        { eraStart =
                            Bound
                            { boundTime = RelativeTime 44064000
                            , boundSlot = SlotNo 13694400
                            , boundEpoch = EpochNo 102
                            }
                        , eraEnd =
                            EraEnd Bound
                            { boundTime = RelativeTime 48384000
                            , boundSlot = SlotNo 18014400
                            , boundEpoch = EpochNo 112
                            }
                        , eraParams =
                            EraParams
                            { eraEpochSize = EpochSize 432000
                            , eraSlotLength = mkSlotLength 1
                            , eraSafeZone = StandardSafeZone 129600
                            }
                        }
                        $ OCC.NonEmptyCons
                            EraSummary
                            { eraStart =
                                Bound
                                { boundTime = RelativeTime 48384000
                                , boundSlot = SlotNo 18014400
                                , boundEpoch = EpochNo 112
                                }
                            , eraEnd =
                                EraEnd Bound
                                    { boundTime = RelativeTime 66528000
                                    , boundSlot = SlotNo 36158400
                                    , boundEpoch = EpochNo 154
                                    }
                            , eraParams =
                                EraParams
                                { eraEpochSize = EpochSize 432000
                                , eraSlotLength = mkSlotLength 1
                                , eraSafeZone = StandardSafeZone 129600
                                }
                            }
                            $ OCC.NonEmptyOne
                                EraSummary
                                { eraStart =
                                    Bound
                                    { boundTime = RelativeTime 66528000
                                    , boundSlot = SlotNo 36158400
                                    , boundEpoch = EpochNo 154
                                    }
                                , eraEnd = EraUnbounded
                                , eraParams =
                                    EraParams
                                    { eraEpochSize = EpochSize 432000
                                    , eraSlotLength = mkSlotLength 1
                                    , eraSafeZone = StandardSafeZone 129600
                                    }
                                }
            }
    Testnet magic ->
        error $
            "Epoch/Era conversion isn't provided for the Testnet "
                <> show magic

{- Epoch-to-Era translation is not available in the Blockfrost API.

The following histories are hardcoded in order to work around this limitation:

For the Mainnet:      For the Testnet:
┌───────┬─────────┐   ┌───────┬─────────┐
│ Epoch │   Era   │   │ Epoch │   Era   │
├───────┼─────────┤   ├───────┼─────────┤
│  ...  │ Alonzo  │   │  ...  │ Alonzo  │
│  290  │ Alonzo  │   │  154  │ Alonzo  │
├───────┼─────────┤   ├───────┼─────────┤
│  289  │  Mary   │   │  153  │  Mary   │
│  ...  │  Mary   │   │  ...  │  Mary   │
│  251  │  Mary   │   │  112  │  Mary   │
├───────┼─────────┤   ├───────┼─────────┤
│  250  │ Allegra │   │  111  │ Allegra │
│  ...  │ Allegra │   │  ...  │ Allegra │
│  236  │ Allegra │   │  102  │ Allegra │
├───────┼─────────┤   ├───────┼─────────┤
│  235  │ Shelley │   │  101  │ Shelley │
│  ...  │ Shelley │   │  ...  │ Shelley │
│  208  │ Shelley │   │   74  │ Shelley │
├───────┼─────────┤   ├───────┼─────────┤
│  207  │  Byron  │   │   73  │  Byron  │
│  ...  │  Byron  │   │  ...  │  Byron  │
│    0  │  Byron  │   │    0  │  Byron  │
└───────┴─────────┘   └───────┴─────────┘
-}

eraBoundaries :: NetworkId -> [(AnyCardanoEra, W.EpochNo)]
eraBoundaries networkId =
    [minBound .. maxBound] <&> \era -> (era, epochEraStartsAt era)
  where
    -- When new era is added this function reminds to update itself:
    -- "Pattern match(es) are non-exhaustive"
    epochEraStartsAt :: AnyCardanoEra -> W.EpochNo
    epochEraStartsAt era = W.EpochNo $ case networkId of
        Mainnet ->
            case era of
                AnyCardanoEra AlonzoEra -> 290
                AnyCardanoEra MaryEra -> 251
                AnyCardanoEra AllegraEra -> 236
                AnyCardanoEra ShelleyEra -> 208
                AnyCardanoEra ByronEra -> 0
        Testnet (NetworkMagic 1097911063) ->
            case era of
                AnyCardanoEra AlonzoEra -> 154
                AnyCardanoEra MaryEra -> 112
                AnyCardanoEra AllegraEra -> 102
                AnyCardanoEra ShelleyEra -> 74
                AnyCardanoEra ByronEra -> 0
        Testnet magic ->
            error $
                "Epoch/Era conversion isn't provided for the Testnet "
                    <> show magic
                    <> " in light mode."

