{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.ConfiguredPool
    ( ConfiguredPool (..)
    , configurePools
    )
where

import Prelude

import Cardano.Api
    ( AsType (..)
    , File (..)
    , Key (..)
    , SerialiseAsCBOR (..)
    )
import Cardano.Binary
    ( FromCBOR (..)
    )
import Cardano.CLI.Shelley.Key
    ( VerificationKeyOrFile (..)
    , readVerificationKeyOrFile
    )
import Cardano.Launcher.Node
    ( CardanoNodeConfig (..)
    , NodePort (..)
    )
import Cardano.Ledger.Api
    ( StandardCrypto
    )
import Cardano.Ledger.BaseTypes
    ( Network (Testnet)
    , StrictMaybe (..)
    , textToUrl
    )
import Cardano.Ledger.Shelley.API
    ( ShelleyGenesis (..)
    , ShelleyGenesisStaking (sgsPools)
    )
import Cardano.Wallet.Launch.Cluster.CardanoCLI
    ( cli
    )
import Cardano.Wallet.Launch.Cluster.ClusterEra
    ( ClusterEra
    , clusterEraToString
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    )
import Cardano.Wallet.Launch.Cluster.Faucet
    ( faucetAmt
    , takeFaucet
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( ClusterLog (..)
    , setLoggingName
    )
import Cardano.Wallet.Launch.Cluster.Node.GenNodeConfig
    ( genNodeConfig
    )
import Cardano.Wallet.Launch.Cluster.Node.GenTopology
    ( genTopology
    )
import Cardano.Wallet.Launch.Cluster.Node.NodeParams
    ( NodeParams (NodeParams)
    )
import Cardano.Wallet.Launch.Cluster.Node.Process
    ( withCardanoNodeProcess
    )
import Cardano.Wallet.Launch.Cluster.Node.RunningNode
    ( RunningNode (RunningNode)
    )
import Cardano.Wallet.Launch.Cluster.PoolMetadataServer
    ( PoolMetadataServer (registerMetadataForPoolIndex, urlFromPoolIndex)
    )
import Cardano.Wallet.Launch.Cluster.PoolRecipe
    ( PoolRecipe (PoolRecipe, operatorKeys)
    )
import Cardano.Wallet.Launch.Cluster.Tx
    ( signTx
    , submitTx
    )
import Cardano.Wallet.Launch.Cluster.UnsafeInterval
    ( unsafeUnitInterval
    )
import Cardano.Wallet.Util
    ( HasCallStack
    )
import Control.Lens
    ( over
    )
import Control.Tracer
    ( Tracer (..)
    , traceWith
    )
import Crypto.Hash.Extra
    ( blake2b256
    )
import Data.Foldable
    ( traverse_
    )
import Data.Generics.Labels
    ()
import Data.IntCast
    ( intCast
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Tagged
    ( Tagged (..)
    , retag
    , untag
    )
import Data.Text
    ( Text
    )
import Data.Word.Odd
    ( Word31
    )
import System.Directory
    ( createDirectoryIfMissing
    )
import System.FilePath
    ( (</>)
    )
import Test.Utils.StaticServer
    ( withStaticServer
    )

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Codec.CBOR.Read as CBOR
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ListMap as ListMap
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Represents the notion of a fully configured pool. All keys are known, but
-- not necessarily exposed using this interface.
data ConfiguredPool = ConfiguredPool
    { operatePool
        :: forall a
         . NodeParams
        -> (RunningNode -> IO a)
        -> IO a
    -- ^ Precondition: the pool must first be registered.
    , metadataUrl
        :: Text
    , recipe
        :: PoolRecipe
    -- ^ The 'PoolRecipe' used to create this 'ConfiguredPool'.
    , registerViaShelleyGenesis
        :: IO (ShelleyGenesis StandardCrypto -> ShelleyGenesis StandardCrypto)
    , finalizeShelleyGenesisSetup :: RunningNode -> IO ()
    -- ^ Submit any pool retirement certificate according to the 'recipe'
    -- on-chain.
    }

configurePools
    :: Config
    -> PoolMetadataServer
    -> NonEmpty PoolRecipe
    -> IO (NonEmpty ConfiguredPool)
configurePools config metadataServer =
    traverse (configurePool config metadataServer)

-- | Create a key pair for a node KES operational key
genKesKeyPair
    :: Tracer IO ClusterLog
    -> Tagged "pool" FilePath
    -> IO (Tagged "kes-prv" FilePath, Tagged "kes-pub" FilePath)
genKesKeyPair tr poolDir = do
    let kesPrv = Tagged @"kes-prv" $ untag poolDir </> "kes.prv"
    let kesPub = Tagged @"kes-pub" $ untag poolDir </> "kes.pub"
    cli
        tr
        [ "node"
        , "key-gen-KES"
        , "--verification-key-file"
        , untag kesPub
        , "--signing-key-file"
        , untag kesPrv
        ]
    pure (kesPrv, kesPub)

-- | Create a key pair for a node VRF operational key
genVrfKeyPair
    :: Tracer IO ClusterLog
    -> Tagged "pool" FilePath
    -> IO (Tagged "vrf-prv" FilePath, Tagged "vrf-pub" FilePath)
genVrfKeyPair tr poolDir = do
    let vrfPrv = Tagged @"vrf-prv" $ untag poolDir </> "vrf.prv"
    let vrfPub = Tagged @"vrf-pub" $ untag poolDir </> "vrf.pub"
    cli
        tr
        [ "node"
        , "key-gen-VRF"
        , "--verification-key-file"
        , untag vrfPub
        , "--signing-key-file"
        , untag vrfPrv
        ]
    pure (vrfPrv, vrfPub)

-- | Write a key pair for a node operator's offline key and a new certificate
-- issue counter
writeOperatorKeyPair
    :: Tracer IO ClusterLog
    -> Tagged "pool" FilePath
    -> PoolRecipe
    -> IO
        ( Tagged "op-prv" FilePath
        , Tagged "op-pub" FilePath
        , Tagged "op-cnt" FilePath
        )
writeOperatorKeyPair tr poolDir recipe = do
    let (_pId, pub, prv, count) = operatorKeys recipe
    traceWith tr $ MsgGenOperatorKeyPair $ untag poolDir

    let opPub = untag poolDir </> "op.pub"
    let opPrv = untag poolDir </> "op.prv"
    let opCount = untag poolDir </> "op.count"

    Aeson.encodeFile opPub pub
    Aeson.encodeFile opPrv prv
    Aeson.encodeFile opCount count

    pure
        ( Tagged @"op-prv" opPrv
        , Tagged @"op-pub" opPub
        , Tagged @"op-cnt" opCount
        )

-- | Issue a node operational certificate
issueOpCert
    :: Tracer IO ClusterLog
    -> Tagged "pool" FilePath
    -> Tagged "kes-pub" FilePath
    -> Tagged "op-prv" FilePath
    -> Tagged "op-cnt" FilePath
    -> IO FilePath
issueOpCert tr nodeDir kesPub opPrv opCount = do
    let file = untag nodeDir </> "op.cert"
    cli
        tr
        [ "node"
        , "issue-op-cert"
        , "--kes-verification-key-file"
        , untag kesPub
        , "--cold-signing-key-file"
        , untag opPrv
        , "--operational-certificate-issue-counter-file"
        , untag opCount
        , "--kes-period"
        , "0"
        , "--out-file"
        , file
        ]
    pure file

-- | Create a stake address key pair
genStakeAddrKeyPair
    :: Tracer IO ClusterLog
    -> (Tagged "stake-prv" FilePath, Tagged "stake-pub" FilePath)
    -> IO ()
genStakeAddrKeyPair tr (stakePrv, stakePub) = do
    cli
        tr
        [ "stake-address"
        , "key-gen"
        , "--verification-key-file"
        , untag stakePub
        , "--signing-key-file"
        , untag stakePrv
        ]

stakePoolIdFromOperatorVerKey
    :: HasCallStack
    => Tagged "op-pub" FilePath
    -> IO (Ledger.KeyHash 'Ledger.StakePool (StandardCrypto))
stakePoolIdFromOperatorVerKey opPub = do
    stakePoolVerKey <-
        either (error . show) id
            <$> readVerificationKeyOrFile
                AsStakePoolKey
                (VerificationKeyFilePath $ File $ untag opPub)
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure
        $ either (error . show) snd
        $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

poolVrfFromFile
    :: HasCallStack
    => Tagged "vrf-pub" FilePath
    -> IO (Ledger.Hash StandardCrypto (Ledger.VerKeyVRF StandardCrypto))
poolVrfFromFile vrfPub = do
    stakePoolVerKey <-
        either (error . show) id
            <$> readVerificationKeyOrFile
                AsVrfKey
                (VerificationKeyFilePath $ File $ untag vrfPub)
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure
        $ either (error . show) snd
        $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

stakingKeyHashFromFile
    :: HasCallStack
    => Tagged "stake-pub" FilePath
    -> IO (Ledger.KeyHash 'Ledger.Staking StandardCrypto)
stakingKeyHashFromFile stakePub = do
    stakePoolVerKey <-
        either (error . show) id
            <$> readVerificationKeyOrFile
                AsStakeKey
                (VerificationKeyFilePath $ File $ untag stakePub)
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure
        $ either (error . show) snd
        $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

stakingAddrFromVkFile
    :: HasCallStack
    => Tagged "stake-pub" FilePath
    -> IO (Ledger.Addr StandardCrypto)
stakingAddrFromVkFile stakePub = do
    stakePoolVerKey <-
        either (error . show) id
            <$> readVerificationKeyOrFile
                AsStakeKey
                (VerificationKeyFilePath $ File $ untag stakePub)
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    let payKH =
            either (error . show) snd
                $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)
    let delegKH =
            either (error . show) snd
                $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)
    pure
        $ Ledger.Addr
            Testnet
            (Ledger.KeyHashObj payKH)
            (Ledger.StakeRefBase (Ledger.KeyHashObj delegKH))

preparePoolRetirement
    :: Tracer IO ClusterLog
    -> Tagged "pool" FilePath
    -> Tagged "cluster-configs" FilePath
    -> ClusterEra
    -> [Tagged "retirement-cert" FilePath]
    -> IO (Tagged "retirement-tx" FilePath, Tagged "faucet-prv" FilePath)
preparePoolRetirement tr poolDir setupDir era certs = do
    let file = untag poolDir </> "tx.raw"
    (faucetInput, faucetPrv) <- takeFaucet setupDir
    cli tr
        $ [ clusterEraToString era
          , "transaction"
          , "build-raw"
          , "--tx-in"
          , untag faucetInput
          , "--ttl"
          , "400"
          , "--fee"
          , show faucetAmt
          , "--out-file"
          , file
          ]
            ++ mconcat ((\cert -> ["--certificate-file", untag cert]) <$> certs)

    pure (Tagged file, faucetPrv)

issuePoolRetirementCert
    :: Tracer IO ClusterLog
    -> Tagged "pool" FilePath
    -> Tagged "op-pub" FilePath
    -> Word31
    -> IO (Tagged "retirement-cert" FilePath)
issuePoolRetirementCert tr poolDir opPub retirementEpoch = do
    let file = untag poolDir </> "pool-retirement.cert"
    cli
        tr
        [ "stake-pool"
        , "deregistration-certificate"
        , "--cold-verification-key-file"
        , untag opPub
        , "--epoch"
        , show retirementEpoch
        , "--out-file"
        , file
        ]
    pure $ Tagged @"retirement-cert" file

configurePool
    :: HasCallStack
    => Config
    -> PoolMetadataServer
    -> PoolRecipe
    -> IO ConfiguredPool
configurePool config@Config{..} metadataServer recipe = do
    let PoolRecipe pledgeAmt i mretirementEpoch metadata _ _ = recipe

    -- Use pool-specific dir
    let name = "pool-" <> show i
    let poolDir :: Tagged "pool" FilePath
        poolDir = Tagged $ untag cfgClusterDir </> name
    createDirectoryIfMissing False (untag poolDir)

    -- Generate/assign keys
    (vrfPrv, vrfPub) <- genVrfKeyPair cfgTracer poolDir
    (kesPrv, kesPub) <- genKesKeyPair cfgTracer poolDir
    (opPrv, opPub, opCount) <- writeOperatorKeyPair cfgTracer poolDir recipe
    opCert <- issueOpCert cfgTracer poolDir kesPub opPrv opCount
    let ownerPub = Tagged @"stake-pub" $ untag poolDir </> "stake.pub"
    let ownerPrv = Tagged @"stake-prv" $ untag poolDir </> "stake.prv"
    genStakeAddrKeyPair cfgTracer (ownerPrv, ownerPub)

    let metadataURL = urlFromPoolIndex metadataServer i
    registerMetadataForPoolIndex metadataServer i metadata
    let metadataBytes = Aeson.encode metadata

    pure
        ConfiguredPool
            { operatePool = \nodeParams action -> do
                let NodeParams genesisFiles hardForks (port, peers) logCfg =
                        nodeParams
                let logCfg' = setLoggingName name logCfg

                topology <-
                    genTopology (retag @"pool" @_ @"output" poolDir) peers
                withStaticServer (untag poolDir) $ \url -> do
                    traceWith cfgTracer $ MsgStartedStaticServer (untag poolDir) url

                    (nodeConfig, genesisData, vd) <-
                        genNodeConfig
                            (retag @"pool" @_ @"output" poolDir)
                            cfgClusterConfigs
                            (Tagged @"node-name" mempty)
                            genesisFiles
                            hardForks
                            logCfg'

                    let cfg =
                            CardanoNodeConfig
                                { nodeDir = untag @"pool" poolDir
                                , nodeConfigFile = untag @"node-config" nodeConfig
                                , nodeTopologyFile = untag @"topology" topology
                                , nodeDatabaseDir = "db"
                                , nodeDlgCertFile = Nothing
                                , nodeSignKeyFile = Nothing
                                , nodeOpCertFile = Just opCert
                                , nodeKesKeyFile = Just $ untag @"kes-prv" kesPrv
                                , nodeVrfKeyFile = Just $ untag @"vrf-prv" vrfPrv
                                , nodePort = Just (NodePort port)
                                , nodeLoggingHostname = Just name
                                , nodeExecutable = Nothing
                                }

                    withCardanoNodeProcess cfgTracer name cfg $ \socket -> do
                        action $ RunningNode socket genesisData vd
            , registerViaShelleyGenesis = do
                poolId <- stakePoolIdFromOperatorVerKey opPub
                vrf <- poolVrfFromFile vrfPub
                stakePubHash <- stakingKeyHashFromFile ownerPub
                pledgeAddr <- stakingAddrFromVkFile ownerPub

                let params =
                        Ledger.PoolParams
                            { ppId = poolId
                            , ppVrf = vrf
                            , ppPledge = Ledger.Coin $ intCast pledgeAmt
                            , ppCost = Ledger.Coin 0
                            , ppMargin = unsafeUnitInterval 0.1
                            , ppRewardAcnt =
                                Ledger.RewardAcnt Testnet
                                    $ Ledger.KeyHashObj stakePubHash
                            , ppOwners = Set.fromList [stakePubHash]
                            , ppRelays = mempty
                            , ppMetadata =
                                SJust
                                    $ Ledger.PoolMetadata
                                        ( fromMaybe (error "invalid url (too long)")
                                            $ textToUrl
                                            $ T.pack metadataURL
                                        )
                                        (blake2b256 (BL.toStrict metadataBytes))
                            }

                let updateStaking sgs =
                        sgs
                            { Ledger.sgsPools =
                                ListMap.ListMap [(poolId, params)] <> sgsPools sgs
                            , Ledger.sgsStake =
                                ListMap.fromList [(stakePubHash, poolId)]
                                    <> Ledger.sgsStake sgs
                            }
                let poolSpecificFunds =
                        ListMap.fromList
                            [(pledgeAddr, Ledger.Coin $ intCast pledgeAmt)]

                pure
                    $ over #sgInitialFunds (poolSpecificFunds <>)
                        . over #sgStaking updateStaking
            , finalizeShelleyGenesisSetup = \(RunningNode socket _ _) -> do
                -- Here is our chance to respect the 'retirementEpoch' of
                -- the 'PoolRecipe'.
                --
                -- NOTE: We also submit the retirement cert in
                -- @registerViaTx@, but this seems to work regardless. (We
                -- do want to submit it here for the sake of babbage)
                let retire e = do
                        retCert <- issuePoolRetirementCert cfgTracer poolDir opPub e
                        (rawTx, faucetPrv) <-
                            preparePoolRetirement
                                cfgTracer
                                poolDir
                                cfgClusterConfigs
                                cfgLastHardFork
                                [retCert]
                        tx <-
                            signTx
                                config
                                (retag @"pool" @_ @"output" poolDir)
                                (retag @"retirement-tx" @_ @"tx-body" rawTx)
                                [ retag @"faucet-prv" @_ @"signing-key" faucetPrv
                                , retag @"stake-prv" @_ @"signing-key" ownerPrv
                                , retag @"op-prv" @_ @"signing-key" opPrv
                                ]
                        submitTx config socket "retirement cert" tx

                traverse_ retire mretirementEpoch
            , metadataUrl = T.pack metadataURL
            , recipe = recipe
            }
