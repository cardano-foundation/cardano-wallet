{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Node.GenesisFiles
    ( GenesisFiles
    , GenesisRecord (..)
    , GenesisTemplateMods
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
    , EpochSize (..)
    , Network (Testnet)
    , natVersion
    )
import Cardano.Ledger.Shelley.API
    ( ShelleyGenesis (..)
    )
import Cardano.Wallet.Launch.Cluster.Aeson
    ( ChangeValue
    , decodeFileThrow
    )
import Cardano.Wallet.Launch.Cluster.ClusterEra
    ( ClusterEra (..)
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
import Data.Aeson
    ( ToJSON (..)
    , Value
    , encodeFile
    )
import Data.Aeson.Lens
    ( key
    )
import Data.Functor.Const
    ( Const (..)
    )
import Data.Generics.Labels
    ()
import Data.HKD
    ( FFoldable (ffoldMap)
    , FFunctor (..)
    , FTraversable (..)
    , FZip (..)
    , ffmapDefault
    , ffoldMapDefault
    )
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
import GHC.Generics
    ( Generic
    )
import System.Path
    ( relFile
    , (</>)
    )

import qualified Cardano.Ledger.Api.Tx.Address as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Data.ListMap as ListMap

data GenesisRecord f = GenesisRecord
    { byronGenesis :: f "genesis-byron"
    , shelleyGenesis :: f "genesis-shelley"
    , alonzoGenesis :: f "genesis-alonzo"
    , conwayGenesis :: Maybe (f "genesis-conway")
    }
    deriving stock (Generic)

instance FFunctor GenesisRecord where ffmap = ffmapDefault
instance FFoldable GenesisRecord where ffoldMap = ffoldMapDefault
instance FTraversable GenesisRecord where
    ftraverse f (GenesisRecord a b c d) =
        GenesisRecord <$> f a <*> f b <*> f c <*> traverse f d

instance FZip GenesisRecord where
    fzipWith f (GenesisRecord a b c md) (GenesisRecord a' b' c' md') =
        GenesisRecord (f a a') (f b b') (f c c') $ do
            f <$> md <*> md'

deriving stock instance Show (GenesisRecord FileOf)

type GenesisFiles = GenesisRecord FileOf

type GenesisTemplateMods = GenesisRecord (Const ChangeValue)

type GenesisValue = GenesisRecord (Const Value)

-- | Read genesis files from disk into json values
readGenesis :: GenesisFiles -> IO GenesisValue
readGenesis = ftraverse readGenesisFile
  where
    readGenesisFile :: FileOf a -> IO (Const Value a)
    readGenesisFile (FileOf fp) = Const <$> decodeFileThrow (toFilePath fp)

-- | Apply template modifications to genesis values
applyTemplateMods :: GenesisTemplateMods -> GenesisValue -> GenesisValue
applyTemplateMods = fzipWith (\(Const f) (Const v) -> Const $ f v)

-- | Write genesis values to disk
writeGenesis :: GenesisFiles -> GenesisValue -> IO ()
writeGenesis fs vs =
    ffoldMapDefault getConst
        $ fzipWith
            (\(FileOf fp) (Const v) -> Const $ encodeFile (toFilePath fp) v)
            fs
            vs

withConway :: ClusterEra -> a -> Maybe a
withConway ConwayHardFork a = Just a
withConway _ _ = Nothing

-- | Create genesis absolute file paths from a directory
mkGenesisFiles :: ClusterEra -> DirOf s -> GenesisFiles
mkGenesisFiles wc (DirOf d) =
    GenesisRecord
        { byronGenesis = mkFile "byron"
        , shelleyGenesis = mkFile "shelley"
        , alonzoGenesis = mkFile "alonzo"
        , conwayGenesis = withConway wc $ mkFile "conway"
        }
  where
    mkFile :: String -> FileOf x
    mkFile x = FileOf $ d </> relFile (x <> "-genesis.json")

-- | Read genesis files from template directory, apply template modifications
--   and write them back to the config directory
produceGenesis
    :: ClusterEra
    -> DirOf template
    -> DirOf configs
    -> GenesisTemplateMods
    -> IO GenesisFiles
produceGenesis wc templateDir configsDir mods = do
    let templates = mkGenesisFiles wc templateDir
    let configs = mkGenesisFiles wc configsDir
    readGenesis templates >>= writeGenesis configs . applyTemplateMods mods
    pure configs

generateGenesis
    :: HasCallStack
    => ClusterEra
    -> [(Address, Coin)]
    -> [ShelleyGenesisModifier]
    -> ClusterM GenesisFiles
generateGenesis wc initialFunds genesisMods = do
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
                          sgGenDelegs = mempty
                        }
                    genesisMods

        produceGenesis wc cfgClusterConfigs cfgClusterDir
            $ GenesisRecord
                { byronGenesis =
                    Const
                        $ key "startTime"
                            .~ toJSON
                                (round @_ @Int $ utcTimeToPOSIXSeconds systemStart)
                , shelleyGenesis = Const
                    $ \_ -> toJSON shelleyGenesisData
                , alonzoGenesis = Const id
                , conwayGenesis = withConway wc $ Const id
                }
