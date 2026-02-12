{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Data types relating to genesis data.
module Cardano.Wallet.Read.Chain.Genesis
    ( -- * Genesis
      GenesisData
    , GenesisHash
    , Byron.GenesisDataError
    , readGenesisData
    , genesisHashMainnet
    , mockGenesisDataMainnet

      -- * NetworkId
    , NetworkId (Mainnet, Testnet)
    , NetworkMagic (..)
    , getNetworkId
    ) where

import Cardano.Crypto.Hashing
    ( decodeAbstractHash
    )
import Cardano.Crypto.ProtocolMagic
    ( ProtocolMagicId (..)
    )
import Control.Monad.Trans.Except
    ( runExceptT
    )
import Data.Time.Clock
    ( UTCTime (..)
    )
import Data.Word
    ( Word32
    )
import GHC.Generics
    ( Generic
    )
import Prelude

import Cardano.Chain.Common qualified as Byron
import Cardano.Chain.Genesis qualified as Byron
import Cardano.Chain.Slotting qualified as Byron
import Cardano.Chain.Update qualified as Byron

{-----------------------------------------------------------------------------
    GenesisData
------------------------------------------------------------------------------}

-- | Initial data required to start the blockchain.
--
-- Contains the initial protocol parameters and allocation of funds.
type GenesisData = Byron.GenesisData

-- | Parse 'GenesisData' from a JSON file and annotate with
-- a canonical JSON hash.
readGenesisData
    :: FilePath
    -> IO (Either Byron.GenesisDataError (GenesisData, GenesisHash))
readGenesisData = runExceptT . Byron.readGenesisData

-- | Hash of the 'GenesisData'.
type GenesisHash = Byron.GenesisHash

-- | The canonical hash of the Cardano mainnet genesis file.
genesisHashMainnet :: GenesisHash
genesisHashMainnet =
    case decodeAbstractHash hex of
        Left _ -> error "genesisHashMainnet malformed"
        Right hash -> Byron.GenesisHash hash
  where
    -- Base 16 / hexadecimal encoded
    hex = "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"

-- | Mock genesis data for testing.
--
-- Does not contain any funds.
-- Does /not/ match 'genesisHashMainnet'.
mockGenesisDataMainnet :: GenesisData
mockGenesisDataMainnet =
    Byron.GenesisData
        { gdGenesisKeyHashes = mempty
        , gdHeavyDelegation = Byron.UnsafeGenesisDelegation mempty
        , gdStartTime = UTCTime (toEnum 0) 0
        , gdNonAvvmBalances = mempty
        , gdProtocolParameters = protocolParametersMainnet
        , gdK = toEnum 2160
        , gdProtocolMagicId = Byron.mainnetProtocolMagicId
        , gdAvvmDistr = Byron.GenesisAvvmBalances mempty
        }

-- Faithful copy of the protocol parameters
-- from the Cardano mainnet genesis file.
protocolParametersMainnet :: Byron.ProtocolParameters
protocolParametersMainnet =
    Byron.ProtocolParameters
        { ppHeavyDelThd = mkLovelacePortionFromGenesisJSON 300000000000
        , ppMaxBlockSize = 2000000
        , ppMaxHeaderSize = 2000000
        , ppMaxProposalSize = 700
        , ppMaxTxSize = 4096
        , ppMpcThd = mkLovelacePortionFromGenesisJSON 20000000000000
        , ppScriptVersion = 0
        , ppSlotDuration = 20000
        , ppSoftforkRule =
            Byron.SoftforkRule
                { srInitThd = mkLovelacePortionFromGenesisJSON 900000000000000
                , srMinThd = mkLovelacePortionFromGenesisJSON 600000000000000
                , srThdDecrement = mkLovelacePortionFromGenesisJSON 50000000000000
                }
        , ppTxFeePolicy =
            Byron.TxFeePolicyTxSizeLinear
                $ Byron.TxSizeLinear
                    (Byron.mkKnownLovelace @155381000000000) -- don't ask
                    43946000000
        , ppUnlockStakeEpoch = Byron.EpochNumber 18446744073709551615
        , ppUpdateProposalThd = mkLovelacePortionFromGenesisJSON 100000000000000
        , ppUpdateVoteThd = mkLovelacePortionFromGenesisJSON 1000000000000
        , ppUpdateProposalTTL = 1000 -- "updateImplicit" in JSON representation
        }

-- Make a 'LovelacePortion' from the numerical value in the genesis JSON file.
mkLovelacePortionFromGenesisJSON :: Rational -> Byron.LovelacePortion
mkLovelacePortionFromGenesisJSON n =
    Byron.rationalToLovelacePortion (n / 10 ^ (15 :: Integer))

{-----------------------------------------------------------------------------
    Network ID
------------------------------------------------------------------------------}

-- | Identification of a Cardano blockchain network.
--
-- * 'Mainnet' refers to the Cardano mainnet.
-- * 'Testnet' refers to a testing network. These networks are distinguished
--   by 'NetworkMagic'.
data NetworkId
    = Mainnet
    | Testnet NetworkMagic
    deriving (Eq, Ord, Generic, Show)

-- | Magic number that identifiers a Cardano testing network.
--
-- (Mainnet has a magic number, too, but please use 'NetworkId'
-- for distinguishing mainnet from testnets.)
newtype NetworkMagic = NetworkMagic {unNetworkMagic :: Word32}
    deriving (Eq, Ord, Enum, Generic, Show)

-- | Get the 'NetworkId' for the given 'GenesisData'.
getNetworkId :: GenesisData -> NetworkId
getNetworkId genesisData =
    if magic == Byron.mainnetProtocolMagicId
        then Mainnet
        else Testnet (NetworkMagic magicword)
  where
    magic@(ProtocolMagicId magicword) = Byron.gdProtocolMagicId genesisData
