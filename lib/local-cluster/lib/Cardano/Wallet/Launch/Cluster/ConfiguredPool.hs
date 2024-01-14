{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
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
    , HasTextEnvelope
    , Key (..)
    , SerialiseAsBech32
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
    ( clusterEraToString
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    , UnliftClusterM (..)
    , askNodeDir
    , askUnliftClusterM
    , traceClusterLog
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    , NodeSegment (..)
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
    ( signAndSubmitTx
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
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Monad.Reader
    ( MonadReader (..)
    )
import Control.Tracer
    ( traceWith
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
import GHC.TypeLits
    ( Symbol
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
    :: PoolMetadataServer
    -> NonEmpty PoolRecipe
    -> ClusterM (NonEmpty ConfiguredPool)
configurePools metadataServer =
    traverse (configurePool metadataServer)

-- | Create a key pair for a node KES operational key
genKesKeyPair
    :: NodeSegment
    -> ClusterM (Tagged "kes-prv" FilePath, Tagged "kes-pub" FilePath)
genKesKeyPair nodeSegment = do
    poolDir <- askNodeDir nodeSegment
    let kesPrv = Tagged @"kes-prv" $ poolDir </> "kes.prv"
    let kesPub = Tagged @"kes-pub" $ poolDir </> "kes.pub"
    cli
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
    :: NodeSegment
    -> ClusterM (Tagged "vrf-prv" FilePath, Tagged "vrf-pub" FilePath)
genVrfKeyPair nodeSegment = do
    poolDir <- askNodeDir nodeSegment
    let vrfPrv = Tagged @"vrf-prv" $ poolDir </> "vrf.prv"
    let vrfPub = Tagged @"vrf-pub" $ poolDir </> "vrf.pub"
    cli
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
    :: NodeSegment
    -> PoolRecipe
    -> ClusterM
        ( Tagged "op-prv" FilePath
        , Tagged "op-pub" FilePath
        , Tagged "op-cnt" FilePath
        )
writeOperatorKeyPair nodeSegment recipe = do
    poolDir <- askNodeDir nodeSegment
    let (_pId, pub, prv, count) = operatorKeys recipe
    traceClusterLog $ MsgGenOperatorKeyPair poolDir

    let opPub = poolDir </> "op.pub"
    let opPrv = poolDir </> "op.prv"
    let opCount = poolDir </> "op.count"

    liftIO $ do
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
    :: NodeSegment
    -> Tagged "kes-pub" FilePath
    -> Tagged "op-prv" FilePath
    -> Tagged "op-cnt" FilePath
    -> ClusterM FilePath
issueOpCert nodeSegment kesPub opPrv opCount = do
    nodeDir <- askNodeDir nodeSegment
    let file = nodeDir </> "op.cert"
    cli
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
    :: (Tagged "stake-prv" FilePath, Tagged "stake-pub" FilePath)
    -> ClusterM ()
genStakeAddrKeyPair (stakePrv, stakePub) = do
    cli
        [ "stake-address"
        , "key-gen"
        , "--verification-key-file"
        , untag stakePub
        , "--signing-key-file"
        , untag stakePrv
        ]

readFailVerificationKeyOrFile
    :: forall keyrole (s :: Symbol)
     . ( HasTextEnvelope (VerificationKey keyrole)
       , SerialiseAsBech32 (VerificationKey keyrole)
       )
    => AsType keyrole
    -> Tagged s FilePath
    -> ClusterM (VerificationKey keyrole)
readFailVerificationKeyOrFile role op =
    liftIO
        $ either (error . show) id
            <$> readVerificationKeyOrFile
                role
                (VerificationKeyFilePath $ File $ untag op)

stakePoolIdFromOperatorVerKey
    :: HasCallStack
    => Tagged "op-pub" FilePath
    -> ClusterM (Ledger.KeyHash 'Ledger.StakePool (StandardCrypto))
stakePoolIdFromOperatorVerKey opPub = do
    stakePoolVerKey <- readFailVerificationKeyOrFile AsStakePoolKey opPub
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure
        $ either (error . show) snd
        $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

poolVrfFromFile
    :: HasCallStack
    => Tagged "vrf-pub" FilePath
    -> ClusterM (Ledger.Hash StandardCrypto (Ledger.VerKeyVRF StandardCrypto))
poolVrfFromFile vrfPub = do
    stakePoolVerKey <- readFailVerificationKeyOrFile AsVrfKey vrfPub
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure
        $ either (error . show) snd
        $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

stakingKeyHashFromFile
    :: HasCallStack
    => Tagged "stake-pub" FilePath
    -> ClusterM (Ledger.KeyHash 'Ledger.Staking StandardCrypto)
stakingKeyHashFromFile stakePub = do
    stakePoolVerKey <- readFailVerificationKeyOrFile AsStakeKey stakePub
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure
        $ either (error . show) snd
        $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

stakingAddrFromVkFile
    :: HasCallStack
    => Tagged "stake-pub" FilePath
    -> ClusterM (Ledger.Addr StandardCrypto)
stakingAddrFromVkFile stakePub = do
    stakePoolVerKey <- readFailVerificationKeyOrFile AsStakeKey stakePub
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
    :: NodeSegment
    -> [Tagged "retirement-cert" FilePath]
    -> ClusterM (Tagged "retirement-tx" FilePath, Tagged "faucet-prv" FilePath)
preparePoolRetirement nodeSegment certs = do
    Config{..} <- ask
    poolDir <- askNodeDir nodeSegment
    let file = poolDir </> "tx.raw"
    (faucetInput, faucetPrv) <- takeFaucet
    cli
        $ [ clusterEraToString cfgLastHardFork
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
    :: NodeSegment
    -> Tagged "op-pub" FilePath
    -> Word31
    -> ClusterM (Tagged "retirement-cert" FilePath)
issuePoolRetirementCert nodeSegment opPub retirementEpoch = do
    poolDir <- askNodeDir nodeSegment
    let file = poolDir </> "pool-retirement.cert"
    cli
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
    => PoolMetadataServer
    -> PoolRecipe
    -> ClusterM ConfiguredPool
configurePool metadataServer recipe = do
    let PoolRecipe pledgeAmt i mretirementEpoch metadata _ _ = recipe

    UnliftClusterM withConfig Config{..} <- askUnliftClusterM
    -- Use pool-specific dir
    let name = "pool-" <> show i
    let nodeSegment = NodeSegment name
    poolDir <- askNodeDir nodeSegment
    liftIO $ createDirectoryIfMissing False poolDir

    -- Generate/assign keys
    (vrfPrv, vrfPub) <- genVrfKeyPair nodeSegment
    (kesPrv, kesPub) <- genKesKeyPair nodeSegment
    (opPrv, opPub, opCount) <- writeOperatorKeyPair nodeSegment recipe
    opCert <- issueOpCert nodeSegment kesPub opPrv opCount
    let ownerPub = Tagged @"stake-pub" $ poolDir </> "stake.pub"
    let ownerPrv = Tagged @"stake-prv" $ poolDir </> "stake.prv"
    genStakeAddrKeyPair (ownerPrv, ownerPub)

    let metadataURL = urlFromPoolIndex metadataServer i
    liftIO $ registerMetadataForPoolIndex metadataServer i metadata
    let metadataBytes = Aeson.encode metadata
    pure
        ConfiguredPool
            { operatePool = \nodeParams action -> do
                let NodeParams genesisFiles hardForks (port, peers) logCfg =
                        nodeParams
                let logCfg' = setLoggingName name logCfg

                topology <- withConfig $ genTopology nodeSegment peers
                withStaticServer poolDir $ \url -> do
                    traceWith cfgTracer $ MsgStartedStaticServer poolDir url

                    (nodeConfig, genesisData, vd) <-
                        withConfig
                            $ genNodeConfig
                                nodeSegment
                                (Tagged @"node-name" mempty)
                                genesisFiles
                                hardForks
                                logCfg'

                    let cfg =
                            CardanoNodeConfig
                                { nodeDir = poolDir
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

                    withConfig
                        $ withCardanoNodeProcess name cfg
                        $ \socket -> action $ RunningNode socket genesisData vd
            , registerViaShelleyGenesis = withConfig $ do
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
                        retCert <- issuePoolRetirementCert nodeSegment opPub e
                        (rawTx, faucetPrv) <-
                            preparePoolRetirement
                                nodeSegment
                                [retCert]
                        signAndSubmitTx
                            socket
                            (Tagged @"output" poolDir)
                            (retag @"retirement-tx" @_ @"tx-body" rawTx)
                            [ retag @"faucet-prv" @_ @"signing-key" faucetPrv
                            , retag @"stake-prv" @_ @"signing-key" ownerPrv
                            , retag @"op-prv" @_ @"signing-key" opPrv
                            ]
                            "retirement cert"

                withConfig $ traverse_ retire mretirementEpoch
            , metadataUrl = T.pack metadataURL
            , recipe = recipe
            }
