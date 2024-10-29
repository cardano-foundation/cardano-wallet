{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Conversion functions and static chain settings for Byron.
module Cardano.Wallet.Primitive.Ledger.Byron
    ( -- * Chain Parameters
      mainnetNetworkParameters
    , maryTokenBundleMaxSize

      -- * Genesis
    , emptyGenesis
    , genesisBlockFromTxOuts

      -- * Conversions
    , fromGenesisData
    , byronCodecConfig
    , fromProtocolMagicId
    , fromByronTxIn
    , fromByronTxOut
    , protocolParametersFromPP
    ) where

import Prelude

import Cardano.Chain.Common
    ( BlockCount (..)
    , Lovelace
    , TxFeePolicy (..)
    , TxSizeLinear (..)
    , unsafeGetLovelace
    )
import Cardano.Chain.Genesis
    ( GenesisData (..)
    , GenesisHash (..)
    , GenesisNonAvvmBalances (..)
    )
import Cardano.Chain.Slotting
    ( EpochSlots (..)
    )
import Cardano.Chain.Update
    ( ProtocolParameters (..)
    )
import Cardano.Chain.UTxO
    ( TxOut (..)
    )
import Cardano.Crypto.ProtocolMagic
    ( ProtocolMagicId
    , unProtocolMagicId
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Inputs
    ( fromByronTxIn
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Outputs
    ( fromByronTxOut
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex
    )
import Cryptography.Hash.Blake
    ( blake2b256
    )
import Data.Coerce
    ( coerce
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime
    )
import Data.Word
    ( Word16
    )
import Numeric.Natural
    ( Natural
    )
import Ouroboros.Consensus.Byron.Ledger
    ( ByronBlock (..)
    )
import Ouroboros.Consensus.Byron.Ledger.Config
    ( CodecConfig (..)
    )
import Ouroboros.Consensus.HardFork.History.Summary
    ( Bound (..)
    )
import Ouroboros.Network.Block
    ( SlotNo (..)
    )

import qualified Cardano.Chain.Update as Update
import qualified Cardano.Crypto.Hashing as CC
import qualified Cardano.Slotting.Slot as Slotting
import qualified Cardano.Wallet.Primitive.Slotting as W
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Block as W
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.EpochNo as W
import qualified Cardano.Wallet.Primitive.Types.EraInfo as W
import qualified Cardano.Wallet.Primitive.Types.FeePolicy as W
import qualified Cardano.Wallet.Primitive.Types.GenesisParameters as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.NetworkParameters as W
import qualified Cardano.Wallet.Primitive.Types.ProtocolMagic as W
import qualified Cardano.Wallet.Primitive.Types.ProtocolParameters as W
import qualified Cardano.Wallet.Primitive.Types.SlottingParameters as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundleMaxSize as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Primitive.Types.Tx.Constraints as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
    ( TxOut (TxOut)
    )
import qualified Cardano.Wallet.Primitive.Types.TxParameters as W
import qualified Data.Map.Strict as Map
import qualified Ouroboros.Consensus.Block as O

--------------------------------------------------------------------------------
--
-- Chain Parameters

mainnetNetworkParameters :: W.NetworkParameters
mainnetNetworkParameters =
    W.NetworkParameters
        { genesisParameters =
            W.GenesisParameters
                { getGenesisBlockHash =
                    W.Hash
                        $ unsafeFromHex
                            "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"
                , getGenesisBlockDate =
                    W.StartTime $ posixSecondsToUTCTime 1_506_203_091
                }
        , slottingParameters =
            W.SlottingParameters
                { getSlotLength =
                    W.SlotLength 20
                , getEpochLength =
                    W.EpochLength 21_600
                , getActiveSlotCoefficient =
                    W.ActiveSlotCoefficient 1.0
                , getSecurityParameter =
                    Quantity 2_160
                }
        , protocolParameters =
            W.ProtocolParameters
                { decentralizationLevel =
                    minBound
                , txParameters =
                    W.TxParameters
                        { getFeePolicy =
                            W.LinearFee
                                $ W.LinearFunction{intercept = 155_381, slope = 43.946}
                        , getTxMaxSize =
                            Quantity 4_096
                        , getTokenBundleMaxSize = maryTokenBundleMaxSize
                        , getMaxExecutionUnits = W.ExecutionUnits 0 0
                        }
                , desiredNumberOfStakePools = 0
                , stakeKeyDeposit = W.Coin 0
                , eras = W.emptyEraInfo
                , -- Collateral inputs were not supported or required in Byron:
                  maximumCollateralInputCount = 0
                , minimumCollateralPercentage = 0
                , executionUnitPrices = Nothing
                }
        }

-- | The max size of token bundles hard-coded in Mary.
--
-- The concept was introduced in Mary, and hard-coded to this value. In Alonzo
-- it became an updateable protocol parameter.
--
-- NOTE: A bit weird to define in "Cardano.Wallet.Primitive.Ledger.Byron", but we
-- need it both here and in "Cardano.Wallet.Primitive.Ledger.Shelley".
maryTokenBundleMaxSize :: W.TokenBundleMaxSize
maryTokenBundleMaxSize = W.TokenBundleMaxSize $ W.TxSize 4_000

-- NOTE
-- For MainNet and TestNet, we can get away with empty genesis blocks with
-- the following assumption:
--
-- - Users won't ever restore a wallet that has genesis UTxO.
--
-- This assumption is _true_ for any user using HD wallets (sequential or
-- random) which means, any user of cardano-wallet.
emptyGenesis :: W.GenesisParameters -> W.Block
emptyGenesis gp =
    W.Block
        { transactions = []
        , delegations = []
        , header =
            W.BlockHeader
                { slotNo =
                    Slotting.SlotNo 0
                , blockHeight =
                    Quantity 0
                , headerHash =
                    coerce $ W.getGenesisBlockHash gp
                , parentHeaderHash =
                    Nothing
                }
        }

--------------------------------------------------------------------------------
--
-- Genesis

-- | Construct a ("fake") genesis block from genesis transaction outputs.
--
-- The genesis data on haskell nodes is not a block at all, unlike the block0 on
-- jormungandr. This function is a method to deal with the discrepancy.
genesisBlockFromTxOuts :: W.GenesisParameters -> [W.TxOut] -> W.Block
genesisBlockFromTxOuts gp outs =
    W.Block
        { delegations = []
        , header =
            W.BlockHeader
                { slotNo =
                    SlotNo 0
                , blockHeight =
                    Quantity 0
                , headerHash =
                    coerce $ W.getGenesisBlockHash gp
                , parentHeaderHash =
                    Nothing
                }
        , transactions = mkTx <$> outs
        }
  where
    mkTx out@(W.TxOut (W.Address bytes) _) =
        W.Tx
            { txId = W.Hash $ blake2b256 bytes
            , txCBOR = Nothing
            , fee = Nothing
            , resolvedInputs = []
            , resolvedCollateralInputs = []
            , outputs = [out]
            , collateralOutput = Nothing
            , withdrawals = mempty
            , metadata = Nothing
            , scriptValidity = Nothing
            }

--------------------------------------------------------------------------------
--
-- Type Conversions

toEpochSlots :: W.EpochLength -> EpochSlots
toEpochSlots =
    EpochSlots . fromIntegral . W.unEpochLength

byronCodecConfig :: W.SlottingParameters -> CodecConfig ByronBlock
byronCodecConfig W.SlottingParameters{getEpochLength} =
    ByronCodecConfig (toEpochSlots getEpochLength)

fromTxFeePolicy :: TxFeePolicy -> W.FeePolicy
fromTxFeePolicy (TxFeePolicyTxSizeLinear (TxSizeLinear a b)) =
    W.LinearFee
        $ W.LinearFunction
            { intercept = lovelaceToDouble a
            , slope = rationalToDouble b
            }
  where
    lovelaceToDouble :: Lovelace -> Double
    lovelaceToDouble = fromIntegral . unsafeGetLovelace

    rationalToDouble :: Rational -> Double
    rationalToDouble = fromRational

fromSlotDuration :: Natural -> W.SlotLength
fromSlotDuration =
    W.SlotLength . toEnum . (* 1_000_000_000) . fromIntegral

-- NOTE: Unsafe conversion from Word64 -> Word32 here.
--
-- Although... Word64 for `k`? For real?
fromBlockCount :: BlockCount -> W.EpochLength
fromBlockCount (BlockCount k) =
    W.EpochLength (10 * fromIntegral k)

-- NOTE: Unsafe conversion from Natural -> Word16
fromMaxSize :: Natural -> Quantity "byte" Word16
fromMaxSize =
    Quantity . fromIntegral

-- | Extract the protocol parameters relevant to the wallet
-- from the Byron 'ProtocolParameters'.
protocolParametersFromPP
    :: W.EraInfo Bound
    -> Update.ProtocolParameters
    -> W.ProtocolParameters
protocolParametersFromPP eraInfo pp =
    W.ProtocolParameters
        { decentralizationLevel = minBound
        , txParameters =
            W.TxParameters
                { getFeePolicy = fromTxFeePolicy $ Update.ppTxFeePolicy pp
                , getTxMaxSize = fromMaxSize $ Update.ppMaxTxSize pp
                , getTokenBundleMaxSize = maryTokenBundleMaxSize
                , getMaxExecutionUnits = W.ExecutionUnits 0 0
                }
        , desiredNumberOfStakePools = 0
        , stakeKeyDeposit = W.Coin 0
        , eras = fromBound <$> eraInfo
        , -- Collateral inputs were not supported or required in Byron:
          maximumCollateralInputCount = 0
        , minimumCollateralPercentage = 0
        , executionUnitPrices = Nothing
        }
  where
    fromBound (Bound _relTime _slotNo (O.EpochNo e)) =
        W.EpochNo $ fromIntegral e

-- | Convert non AVVM balances to genesis UTxO.
fromNonAvvmBalances :: GenesisNonAvvmBalances -> [W.TxOut]
fromNonAvvmBalances (GenesisNonAvvmBalances m) =
    fromByronTxOut . uncurry TxOut <$> Map.toList m

-- | Convert genesis data into blockchain params and an initial set of UTxO
fromGenesisData :: (GenesisData, GenesisHash) -> (W.NetworkParameters, [W.TxOut])
fromGenesisData (genesisData, genesisHash) =
    ( W.NetworkParameters
        { genesisParameters =
            W.GenesisParameters
                { getGenesisBlockHash =
                    W.Hash . CC.hashToBytes . unGenesisHash $ genesisHash
                , getGenesisBlockDate =
                    W.StartTime . gdStartTime $ genesisData
                }
        , slottingParameters =
            W.SlottingParameters
                { getSlotLength =
                    fromSlotDuration . ppSlotDuration
                        $ gdProtocolParameters genesisData
                , getEpochLength = fromBlockCount . gdK $ genesisData
                , getActiveSlotCoefficient = W.ActiveSlotCoefficient 1.0
                , getSecurityParameter =
                    Quantity . fromIntegral . unBlockCount
                        $ gdK genesisData
                }
        , protocolParameters =
            -- emptyEraInfo contains no info about byron. Should we add it?
            protocolParametersFromPP W.emptyEraInfo
                $ gdProtocolParameters genesisData
        }
    , fromNonAvvmBalances . gdNonAvvmBalances $ genesisData
    )

fromProtocolMagicId :: ProtocolMagicId -> W.ProtocolMagic
fromProtocolMagicId = W.ProtocolMagic . fromIntegral . unProtocolMagicId
