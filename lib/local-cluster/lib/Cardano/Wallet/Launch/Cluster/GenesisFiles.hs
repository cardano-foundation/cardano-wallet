{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.GenesisFiles
    ( GenesisFiles (..)
    , generateGenesis
    )
where

import Prelude

import Cardano.Address
    ( Address (..)
    )
import Cardano.Ledger.Api
    ( ppA0L
    , ppDL
    , ppEMaxL
    , ppExtraEntropyL
    , ppKeyDepositL
    , ppMaxBBSizeL
    , ppMaxBHSizeL
    , ppMaxTxSizeL
    , ppMinFeeAL
    , ppMinFeeBL
    , ppMinPoolCostL
    , ppMinUTxOValueL
    , ppNOptL
    , ppPoolDepositL
    , ppProtocolVersionL
    , ppRhoL
    , ppTauL
    )
import Cardano.Ledger.BaseTypes
    ( EpochInterval (..)
    , Network (Testnet)
    , natVersion, EpochSize (..)
    )
import Cardano.Ledger.Shelley.API
    ( ShelleyGenesis (..)
    )
import Cardano.Wallet.Launch.Cluster.Aeson
    ( withAddedKey
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    , ShelleyGenesisModifier
    , TestnetMagic (testnetMagicToNatural)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , FileOf (..)
    , toFilePath
    )
import Cardano.Wallet.Launch.Cluster.UnsafeInterval
    ( unsafeNonNegativeInterval
    , unsafePositiveUnitInterval
    , unsafeUnitInterval
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Util
    ( HasCallStack
    )
import Control.Lens
    ( (&)
    , (.~)
    )
import Control.Monad.Reader
    ( MonadIO (..)
    , MonadReader (..)
    )
import Data.Aeson.QQ
    ( aesonQQ
    )
import Data.Either
    ( fromRight
    )
import Data.Generics.Labels
    ()
import Data.IntCast
    ( intCast
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Time.Clock
    ( addUTCTime
    , getCurrentTime
    )
import Data.Time.Clock.POSIX
    ( utcTimeToPOSIXSeconds
    )
import System.Directory
    ( copyFile
    )
import System.Path hiding
    ( FilePath
    )

import qualified Cardano.Ledger.Api.Tx.Address as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Data.Aeson as Aeson
import qualified Data.ListMap as ListMap

data GenesisFiles = GenesisFiles
    { byronGenesis :: FileOf "genesis-byron"
    , shelleyGenesis :: FileOf "genesis-shelley"
    , alonzoGenesis :: FileOf "genesis-alonzo"
    , conwayGenesis :: FileOf "genesis-conway"
    }
    deriving stock (Show, Eq)

generateGenesis
    :: HasCallStack
    => [(Address, Coin)]
    -> [ShelleyGenesisModifier]
    -> ClusterM GenesisFiles
generateGenesis initialFunds genesisMods = do
    Config{..} <- ask
    {- The timestamp of the 0-th slot.

    Ideally it should be few seconds later than the cluster actually starts.
    If it's significantly later, nodes won't be doing anything for a while.
    If it's slightly before the actual starts, some slots will be missed,
    but it shouldn't be critical as long as less than k slots are missed.

    Empirically, 10 seconds seems to be a good value: enough for a cluster to
    initialize itself before producing any blocks, but not too much to wait for.

    Lower values (e.g. 1 second) might cause custer to start but not produce
    any blocks, because the first slot will be too far in the past. When this
    happens then node logs contain TraceNoLedgerView message and wallet log says
    "Current tip is [point genesis]. (not applying blocks)"
    -}

    liftIO $ do
        systemStart <- addUTCTime 10 <$> getCurrentTime

        let
            sgProtocolParams =
                Ledger.emptyPParams
                    & ppMinFeeAL
                        .~ Ledger.Coin 100
                    & ppMinFeeBL
                        .~ Ledger.Coin 100_000
                    & ppMinUTxOValueL
                        .~ Ledger.Coin 1_000_000
                    & ppKeyDepositL
                        .~ Ledger.Coin 1_000_000
                    & ppPoolDepositL
                        .~ Ledger.Coin 0
                    & ppMaxBBSizeL
                        .~ 239_857
                    & ppMaxBHSizeL
                        .~ 1_100
                    & ppMaxTxSizeL
                        .~ 16_384
                    & ppMinPoolCostL
                        .~ Ledger.Coin 0
                    & ppExtraEntropyL
                        .~ Ledger.NeutralNonce
                    -- There are a few smaller features/fixes which are enabled
                    -- based on the protocol version rather than just the era,
                    -- so we need to set it to a realisitic value.
                    & ppProtocolVersionL
                        .~ Ledger.ProtVer (natVersion @8) 0
                    -- Sensible pool & reward parameters:
                    & ppNOptL
                        .~ 3
                    & ppRhoL
                        .~ unsafeUnitInterval 0.02
                    & ppTauL
                        .~ unsafeUnitInterval 0.1
                    & ppA0L
                        .~ unsafeNonNegativeInterval 0.1
                    & ppDL
                        .~ unsafeUnitInterval 0
                    -- The epoch bound on pool retirements specifies how many epochs
                    -- in advance retirements may be announced. For testing purposes,
                    -- we allow retirements to be announced far into the future.
                    & ppEMaxL
                        .~ EpochInterval 1_000_000

        let shelleyGenesisData =
                foldr
                    ($)
                    ShelleyGenesis
                        { sgSystemStart = systemStart
                        , sgActiveSlotsCoeff = unsafePositiveUnitInterval 0.5
                        , sgSecurityParam = 10
                        , sgEpochLength = EpochSize 120
                        , sgSlotLength = 0.25
                        , sgUpdateQuorum = 1
                        , sgNetworkMagic =
                            fromIntegral (testnetMagicToNatural cfgTestnetMagic)
                        , sgSlotsPerKESPeriod = 86_400
                        , sgMaxKESEvolutions = 5
                        , sgNetworkId = Testnet
                        , sgMaxLovelaceSupply = 1_000_000_000_000_000_000
                        , sgProtocolParams
                        , sgInitialFunds =
                            ListMap.fromList
                                [ ( fromMaybe (error "sgInitialFunds: invalid addr")
                                        $ Ledger.decodeAddrLenient
                                        $ unAddress address
                                  , Ledger.Coin $ intCast c
                                  )
                                | (address, Coin c) <- initialFunds
                                ]
                        , sgStaking = Ledger.emptyGenesisStaking
                        , -- We need this to submit MIR certs
                          -- (and probably for the BFT node pre-babbage):
                          sgGenDelegs =
                            fromRight (error "invalid sgGenDelegs") . Aeson.eitherDecode
                                $ Aeson.encode
                                    [aesonQQ|
                    {"91612ee7b158dc64871a959060973d0f2b8fb6e85ae960f03b8640ac": {
                        "delegate": "180b3fae61789f61cbdbc69e5f8e1beae9093aa2215e482dc8d89ec9",
                        "vrf": "e9ef3b5d81d400eb046de696354ff8e84122f505e706e3c86a361cce919a686e"
                    }}|]
                        }
                    genesisMods

        let byronFileOf, shelleyFileOf, alonzoFileOf, conwayFileOf
                :: DirOf x  -> AbsFile
            byronFileOf x = absDirOf x </> relFile "byron-genesis.json"
            shelleyFileOf x = absDirOf x </> relFile "shelley-genesis.json"
            alonzoFileOf x = absDirOf x </> relFile "alonzo-genesis.json"
            conwayFileOf x = absDirOf x </> relFile "conway-genesis.json"

        let shelleyGenesis = shelleyFileOf cfgClusterDir
        Aeson.encodeFile (toFilePath shelleyGenesis) shelleyGenesisData

        let fileToAeson :: FilePath -> IO Aeson.Value
            fileToAeson f = Aeson.eitherDecodeFileStrict f >>= either fail pure

        let byronGenesis = byronFileOf cfgClusterDir
        fileToAeson (toFilePath $ byronFileOf cfgClusterConfigs)
            >>= withAddedKey
                "startTime"
                (round @_ @Int $ utcTimeToPOSIXSeconds systemStart)
            >>= Aeson.encodeFile (toFilePath byronGenesis)

        let alonzoGenesis = alonzoFileOf cfgClusterDir
        fileToAeson (toFilePath $ alonzoFileOf cfgClusterConfigs)
            >>= Aeson.encodeFile (toFilePath alonzoGenesis)

        let conwayGenesis = conwayFileOf cfgClusterDir
        copyFile
            (toFilePath $ conwayFileOf cfgClusterConfigs)
            (toFilePath conwayGenesis)

        pure
            GenesisFiles
                { byronGenesis = FileOf byronGenesis
                , shelleyGenesis = FileOf shelleyGenesis
                , alonzoGenesis = FileOf alonzoGenesis
                , conwayGenesis = FileOf conwayGenesis
                }
