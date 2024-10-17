{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.DummyTarget.Primitive.Types
    ( -- * Dummy values
      block0
    , dummyNetworkParameters
    , dummyGenesisParameters
    , dummyProtocolParameters
    , dummyLedgerProtocolParameters
    , dummySlottingParameters
    , dummyTimeInterpreter
    , dummyGenesisHash
    , mkTxId
    , mkTx

      -- * Mocks
    , dummyNetworkLayer
    ) where

import Prelude

import Cardano.Wallet.Network
    ( NetworkLayer (..)
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( fromConwayPParams
    )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter
    , hoistTimeInterpreter
    , mkSingleEraInterpreter
    )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , Block (..)
    , BlockHeader (..)
    , EpochLength (..)
    , GenesisParameters (..)
    , NetworkParameters (..)
    , ProtocolParameters (..)
    , SlotLength (..)
    , SlotNo (..)
    , SlottingParameters (..)
    , StartTime (..)
    , emptyEraInfo
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    , mockHash
    )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..)
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..)
    , TxMetadata (..)
    , TxScriptValidity (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..)
    )
import Cardano.Wallet.Read.Tx.CBOR
    ( TxCBOR
    )
import Data.Functor.Identity
    ( Identity (..)
    )
import Data.Map.Strict
    ( Map
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime
    )
import GHC.Stack
    ( HasCallStack
    )
import Internal.Cardano.Write.Tx.Gen
    ( mockPParams
    )

import qualified Cardano.Write.Eras as Write
import qualified Data.ByteString.Char8 as B8
import qualified Internal.Cardano.Write.Tx as Write

{-----------------------------------------------------------------------------
    Dummy values
------------------------------------------------------------------------------}
dummyGenesisHash :: Hash "Genesis"
dummyGenesisHash = Hash (B8.replicate 32 '1')

block0 :: Block
block0 = Block
    { header = BlockHeader
        { slotNo = SlotNo 0
        , blockHeight = Quantity 0
        , headerHash = Hash $ getHash dummyGenesisHash
        , parentHeaderHash = Nothing
        }
    , transactions = []
    , delegations = []
    }

dummyGenesisParameters :: GenesisParameters
dummyGenesisParameters = GenesisParameters
    { getGenesisBlockHash = dummyGenesisHash
    , getGenesisBlockDate = StartTime $ posixSecondsToUTCTime 0
    }

dummySlottingParameters :: SlottingParameters
dummySlottingParameters = SlottingParameters
    { getSlotLength = SlotLength 1
    , getEpochLength = EpochLength 21_600
    , getActiveSlotCoefficient = ActiveSlotCoefficient 1
    , getSecurityParameter = Quantity 2_160
    }

dummyTimeInterpreter :: Monad m => TimeInterpreter m
dummyTimeInterpreter = hoistTimeInterpreter (pure . runIdentity)
    $ mkSingleEraInterpreter
        (getGenesisBlockDate dummyGenesisParameters)
        dummySlottingParameters

dummyNetworkParameters :: NetworkParameters
dummyNetworkParameters = NetworkParameters
    { genesisParameters = dummyGenesisParameters
    , slottingParameters = dummySlottingParameters
    , protocolParameters = dummyProtocolParameters
    }

dummyProtocolParameters :: ProtocolParameters
dummyProtocolParameters = fromConwayPParams
    emptyEraInfo
    (mockPParams @Write.ConwayEra)

dummyLedgerProtocolParameters :: Write.IsRecentEra era => Write.PParams era
dummyLedgerProtocolParameters = mockPParams

dummyNetworkLayer :: HasCallStack => NetworkLayer m a
dummyNetworkLayer = NetworkLayer
    { chainSync = err "chainSync"
    , fetchNextBlock = err "fetchNextBlock"
    , currentNodeEra = err "currentNodeEra"
    , currentNodeTip = err "currentNodeTip"
    , watchNodeTip = err "watchNodeTip"
    , currentProtocolParameters = err "currentProtocolParameters"
    , currentProtocolParametersInRecentEras
        = err "currentProtocolParametersInRecentEras"
    , currentSlottingParameters = err "currentSlottingParameters"
    , getUTxOByTxIn = err "getUTxOByTxIn"
    , getStakeDelegDeposits = error "getStakeDelegDeposits"
    , postSealedTx = err "postSealedTx"
    , postTx = err "postTx"
    , stakeDistribution = err "stakeDistribution"
    , getCachedRewardAccountBalance = err "getRewardCachedAccountBalance"
    , fetchRewardAccountBalances = err "fetchRewardAccountBalances"
    , timeInterpreter = err "timeInterpreter"
    , syncProgress = err "syncProgress"
    }
  where
    err subject = error $
        "`" <> subject <> "` is not implemented in the dummy network layer."

{-----------------------------------------------------------------------------
    Convenience functions
------------------------------------------------------------------------------}
-- | Construct a @Tx@, computing its hash using the dummy @mkTxId@.
mkTx
    :: Maybe TxCBOR
    -> Maybe Coin
    -> [(TxIn, Maybe TxOut)]
    -> [(TxIn, Maybe TxOut)]
    -> [TxOut]
    -> Maybe TxOut
    -> Map RewardAccount Coin
    -> Maybe TxMetadata
    -> Maybe TxScriptValidity
    -> Tx
mkTx cbor fees ins cins outs cout wdrls md validity =
    Tx
      { txId = (mkTxId ins outs wdrls md)
      , txCBOR = cbor
      , fee = fees
      , resolvedInputs = ins
      , resolvedCollateralInputs = cins
      , outputs = outs
      , collateralOutput = cout
      , withdrawals = wdrls
      , metadata = md
      , scriptValidity = validity
      }

-- | txId calculation for testing purposes.
mkTxId
    :: [(TxIn, Maybe TxOut)]
    -> [TxOut]
    -> Map RewardAccount Coin
    -> Maybe TxMetadata -> Hash "Tx"
mkTxId ins outs wdrls md = mockHash (ins, outs, wdrls, md)
