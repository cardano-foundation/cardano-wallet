{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

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
import Cardano.CLI.Types.Key
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
    )
import Cardano.Wallet.Launch.Cluster.Faucet
    ( faucetAmt
    , takeFaucet
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , FileOf (..)
    , absFilePathOf
    , changeFileOf
    , toFilePath
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
    , asks
    )
import Control.Tracer
    ( traceWith
    )
import Cryptography.Hash.Blake
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
import System.Path
    ( RelDir
    , relDir
    , relFile
    , (</>)
    )
import System.Path.Directory
    ( createDirectoryIfMissing
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
        -> (RunningNode -> ClusterM a)
        -> ClusterM a
    -- ^ Precondition: the pool must first be registered.
    , metadataUrl
        :: Text
    , recipe
        :: PoolRecipe
    -- ^ The 'PoolRecipe' used to create this 'ConfiguredPool'.
    , registerViaShelleyGenesis
        :: ClusterM
            (ShelleyGenesis StandardCrypto -> ShelleyGenesis StandardCrypto)
    , finalizeShelleyGenesisSetup :: RunningNode -> ClusterM ()
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
    :: RelDir
    -> ClusterM (FileOf "kes-prv" , FileOf "kes-pub")
genKesKeyPair nodeSegment = do
    DirOf poolDir <- askNodeDir nodeSegment
    let kesPrv = poolDir </> relFile "kes.prv"
        kesPub = poolDir </> relFile "kes.pub"
    cli
        [ "node"
        , "key-gen-KES"
        , "--verification-key-file"
        , toFilePath kesPub
        , "--signing-key-file"
        , toFilePath kesPrv
        ]
    pure (FileOf kesPrv, FileOf kesPub)

-- | Create a key pair for a node VRF operational key
genVrfKeyPair
    :: RelDir
    -> ClusterM (FileOf "vrf-prv" , FileOf "vrf-pub")
genVrfKeyPair nodeSegment = do
    DirOf poolDir <- askNodeDir nodeSegment
    let vrfPrv = poolDir </> relFile "vrf.prv"
        vrfPub = poolDir </> relFile "vrf.pub"
    cli
        [ "node"
        , "key-gen-VRF"
        , "--verification-key-file"
        , toFilePath vrfPub
        , "--signing-key-file"
        , toFilePath vrfPrv
        ]
    pure (FileOf @"vrf-prv" vrfPrv, FileOf @"vrf-pub" vrfPub)

-- | Write a key pair for a node operator's offline key and a new certificate
-- issue counter
writeOperatorKeyPair
    :: RelDir
    -> PoolRecipe
    -> ClusterM
        ( FileOf "op-prv"
        , FileOf "op-pub"
        , FileOf "op-cnt"
        )
writeOperatorKeyPair nodeSegment recipe = do
    poolDirPath@(DirOf poolDir) <- askNodeDir nodeSegment
    let (_pId, pub, prv, count) = operatorKeys recipe
    traceClusterLog $ MsgGenOperatorKeyPair poolDirPath

    let opPub = poolDir </> relFile "op.pub"
    let opPrv = poolDir </> relFile "op.prv"
    let opCount = poolDir </> relFile "op.count"

    liftIO $ do
        Aeson.encodeFile (toFilePath opPub) pub
        Aeson.encodeFile (toFilePath opPrv) prv
        Aeson.encodeFile (toFilePath opCount) count

    pure
        ( FileOf @"op-prv" opPrv
        , FileOf @"op-pub" opPub
        , FileOf @"op-cnt" opCount
        )

-- | Issue a node operational certificate
issueOpCert
    :: RelDir
    -> FileOf "kes-pub"
    -> FileOf "op-prv"
    -> FileOf "op-cnt"
    -> ClusterM (FileOf "op-cert")
issueOpCert nodeSegment (FileOf kesPub) (FileOf opPrv) (FileOf opCount) = do
    DirOf poolDir <- askNodeDir nodeSegment
    let opCertPath = poolDir </> relFile "op.cert"
    cli
        [ "node"
        , "issue-op-cert"
        , "--kes-verification-key-file"
        , toFilePath kesPub
        , "--cold-signing-key-file"
        , toFilePath opPrv
        , "--operational-certificate-issue-counter-file"
        , toFilePath opCount
        , "--kes-period"
        , "0"
        , "--out-file"
        , toFilePath opCertPath
        ]
    pure $ FileOf opCertPath

-- | Create a stake address key pair
genStakeAddrKeyPair
    :: (FileOf "stake-prv", FileOf "stake-pub")
    -> ClusterM ()
genStakeAddrKeyPair (FileOf stakePrv, FileOf stakePub) = do
    Config{..} <- ask
    cli
        [ clusterEraToString cfgLastHardFork
        , "stake-address"
        , "key-gen"
        , "--verification-key-file"
        , toFilePath stakePub
        , "--signing-key-file"
        , toFilePath stakePrv
        ]

readFailVerificationKeyOrFile
    :: forall keyrole (s :: Symbol)
     . ( HasTextEnvelope (VerificationKey keyrole)
       , SerialiseAsBech32 (VerificationKey keyrole)
       )
    => AsType keyrole
    -> FileOf s
    -> ClusterM (VerificationKey keyrole)
readFailVerificationKeyOrFile role (FileOf op) =
    liftIO
        $ either (error . show) id
            <$> readVerificationKeyOrFile
                role
                (VerificationKeyFilePath $ File $ toFilePath op)

stakePoolIdFromOperatorVerKey
    :: HasCallStack
    => FileOf "op-pub"
    -> ClusterM (Ledger.KeyHash 'Ledger.StakePool (StandardCrypto))
stakePoolIdFromOperatorVerKey opPub = do
    stakePoolVerKey <- readFailVerificationKeyOrFile AsStakePoolKey opPub
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure
        $ either (error . show) snd
        $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

poolVrfFromFile
    :: HasCallStack
    => FileOf "vrf-pub"
    -> ClusterM (Ledger.Hash StandardCrypto (Ledger.VerKeyVRF StandardCrypto))
poolVrfFromFile vrfPub = do
    stakePoolVerKey <- readFailVerificationKeyOrFile AsVrfKey vrfPub
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure
        $ either (error . show) snd
        $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

stakingKeyHashFromFile
    :: HasCallStack
    => FileOf "stake-pub"
    -> ClusterM (Ledger.KeyHash 'Ledger.Staking StandardCrypto)
stakingKeyHashFromFile stakePub = do
    stakePoolVerKey <- readFailVerificationKeyOrFile AsStakeKey stakePub
    let bytes = serialiseToCBOR $ verificationKeyHash stakePoolVerKey
    pure
        $ either (error . show) snd
        $ CBOR.deserialiseFromBytes fromCBOR (BL.fromStrict bytes)

stakingAddrFromVkFile
    :: HasCallStack
    => FileOf "stake-pub"
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
    :: RelDir
    -> [FileOf "retirement-cert"]
    -> ClusterM (FileOf "retirement-tx", FileOf "faucet-prv")
preparePoolRetirement nodeSegment certs = do
    Config{..} <- ask
    DirOf poolDir <- askNodeDir nodeSegment
    let transactionFile = poolDir </> relFile "tx.raw"
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
          , toFilePath transactionFile
          ]
            ++ mconcat
                ((\(FileOf cert) -> ["--certificate-file", toFilePath cert])
                    <$> certs
                )

    pure (FileOf transactionFile, faucetPrv)

issuePoolRetirementCert
    :: RelDir -- ^ Node relative path
    -> FileOf "op-pub"
    -> Word31 -- ^ Retirement epoch
    -> ClusterM (FileOf "retirement-cert")
issuePoolRetirementCert nodeSegment (FileOf opPub) retirementEpoch = do
    lastHardFork <- asks cfgLastHardFork
    DirOf poolDir <- askNodeDir nodeSegment
    let file = poolDir </> relFile "pool-retirement.cert"
    cli
        [ clusterEraToString lastHardFork
        , "stake-pool"
        , "deregistration-certificate"
        , "--cold-verification-key-file"
        , toFilePath opPub
        , "--epoch"
        , show retirementEpoch
        , "--out-file"
        , toFilePath file
        ]
    pure $ FileOf @"retirement-cert" file

configurePool
    :: HasCallStack
    => PoolMetadataServer
    -> PoolRecipe
    -> ClusterM ConfiguredPool
configurePool metadataServer recipe = do
    UnliftClusterM withConfig Config{..} <- askUnliftClusterM

    let PoolRecipe pledgeAmt i mRetirementEpoch metadata _ _ = recipe
    liftIO $ registerMetadataForPoolIndex metadataServer i metadata
    -- Use pool-specific dir
    let name = "pool-" <> show i
        nodeRelativePath :: RelDir
        nodeRelativePath = relDir name
    poolDirPath@(DirOf poolDir) <- askNodeDir nodeRelativePath
    liftIO $ createDirectoryIfMissing True poolDir

    -- Generate/assign keys
    (vrfPrv, vrfPub) <- genVrfKeyPair nodeRelativePath
    (kesPrv, kesPub) <- genKesKeyPair nodeRelativePath
    (opPrv, opPub, opCount) <- writeOperatorKeyPair nodeRelativePath recipe
    opCert <- issueOpCert nodeRelativePath kesPub opPrv opCount
    let ownerPub = FileOf @"stake-pub" $ poolDir </> relFile "stake.pub"
    let ownerPrv = FileOf @"stake-prv" $ poolDir </> relFile "stake.prv"
    genStakeAddrKeyPair (ownerPrv, ownerPub)

    let metadataUrl = T.pack $ urlFromPoolIndex metadataServer i
    let metadataBytes = Aeson.encode metadata

        registerViaShelleyGenesis = do
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
                                        $ textToUrl 128 metadataUrl
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

        finalizeShelleyGenesisSetup (RunningNode socket _ _) = do
            -- Here is our chance to respect the 'retirementEpoch' of
            -- the 'PoolRecipe'.
            --
            -- NOTE: We also submit the retirement cert in
            -- @registerViaTx@, but this seems to work regardless. (We
            -- do want to submit it here for the sake of babbage)
            let retire e = do
                    retCert <- issuePoolRetirementCert nodeRelativePath opPub e
                    (rawTx, faucetPrv) <-
                        preparePoolRetirement
                            nodeRelativePath
                            [retCert]
                    signAndSubmitTx
                        socket
                        (changeFileOf @"retirement-tx" @"tx-body" rawTx)
                        [ changeFileOf @"faucet-prv" @"signing-key" faucetPrv
                        , changeFileOf @"stake-prv" @"signing-key" ownerPrv
                        , changeFileOf @"op-prv" @"signing-key" opPrv
                        ]
                        "retirement cert"
            traverse_ retire mRetirementEpoch

        operatePool nodeParams action = do
            let NodeParams
                    genesisFiles
                    hardForks
                    (port, peers)
                    logCfg
                    nodeOutput = nodeParams
            let logCfg' = setLoggingName name logCfg

            topology <- genTopology nodeRelativePath peers
            liftIO $ withStaticServer (toFilePath poolDir) $ \url -> do
                traceWith cfgTracer $ MsgStartedStaticServer url poolDirPath

                (nodeConfig, genesisData, vd) <-
                    withConfig
                        $ genNodeConfig
                            nodeRelativePath
                            (Tagged @"node-name" mempty)
                            genesisFiles
                            hardForks
                            logCfg'

                let cfg = CardanoNodeConfig
                        { nodeDir = toFilePath poolDir
                        , nodeConfigFile = absFilePathOf nodeConfig
                        , nodeTopologyFile = absFilePathOf topology
                        , nodeDatabaseDir = toFilePath
                            $ poolDir </> relDir "db"
                        , nodeDlgCertFile = Nothing
                        , nodeSignKeyFile = Nothing
                        , nodeOpCertFile = Just $ absFilePathOf opCert
                        , nodeKesKeyFile = Just $ absFilePathOf kesPrv
                        , nodeVrfKeyFile = Just $ absFilePathOf vrfPrv
                        , nodePort = Just (NodePort port)
                        , nodeLoggingHostname = Just name
                        , nodeExecutable = Nothing
                        , nodeOutputFile = absFilePathOf <$> nodeOutput
                        }
                withConfig
                    $ withCardanoNodeProcess name cfg
                    $ \socket -> action $ RunningNode socket genesisData vd
    pure ConfiguredPool{..}
